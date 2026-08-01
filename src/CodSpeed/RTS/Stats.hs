{- | Per-benchmark GHC runtime statistics.

CodSpeed's simulation instrument counts instructions; a good proxy for work done,
but for Haskell it is not the sharpest signal available. GHC already maintains an
exactly deterministic one — bytes allocated — and it costs nothing to read. Two
runs of the same code over the same input allocate the same number of bytes, every
time, on every machine. No Valgrind, no 200x slowdown, and it works on platforms
where simulation mode cannot run at all.

So allocation is treated here as a first-class metric rather than a diagnostic.

== Two counters, deliberately

[Allocation]: from 'getAllocationCounter', which is __per-thread__ and accurate to
  the byte. It needs no RTS flag, forces no GC, and — the reason it is preferred
  here — is immune to whatever other threads are doing.

[Collections and copying]: from 'getRTSStats', which is __process-wide__ and needs
  @-T@. Reported as 'Just' when available.

The distinction is not academic. @GHC.Stats.allocated_bytes@ counts every
capability, so any concurrent allocation is charged to whichever measurement
happens to be open — measured here as five runs of an identical action reporting
76256, 63624, 51264, 3776 and 3032 bytes, purely from surrounding test activity.
'getAllocationCounter' reports the same action identically every time.

This is also why the GC fields stay advisory. Use 'measAllocatedBytes' to gate CI;
read 'measGC' for diagnosis, and only trust it when the process is quiet — one
benchmark at a time, which is what @tasty-bench@ enforces by refusing to run
unless @numThreads == 1@.

== Note on the allocation counter

The counter is only read, never set, so any allocation limit installed on the
thread ('GHC.Conc.enableAllocationLimit') keeps working.
-}
module CodSpeed.RTS.Stats (
  -- * Measurements
  Measurement (..),
  GCDelta (..),
  measureRTS,
  gcStatsAvailable,

  -- * Correcting for the bracket's own cost
  AllocationFloor (..),
  calibrateAllocationFloor,
  calibrateAllocationFloorWith,
  subtractFloor,

  -- * Rendering
  renderMeasurement,
) where

import Data.Int (Int64)
import Data.Word (Word32, Word64)
import GHC.Conc.Sync (getAllocationCounter)
import GHC.Stats (
  RTSStats (..),
  getRTSStats,
  getRTSStatsEnabled,
 )
import System.Mem (performGC)

{- | What a measured region did.

'measAllocatedBytes' is exactly reproducible for fixed code and input. Everything
in 'measGC' depends on GC scheduling and on what the rest of the process was
doing.
-}
data Measurement = Measurement
  { measAllocatedBytes :: !Word64
  -- ^ Bytes allocated by the measuring thread. Deterministic, byte-accurate.
  , measGC :: !(Maybe GCDelta)
  -- ^ Collector activity, when @-T@ makes it available.
  }
  deriving (Show, Eq)

-- | Process-wide collector activity across a measured region.
data GCDelta = GCDelta
  { gcCollections :: !Word32
  {- ^ Collections during the region.

  A non-zero count is a quality signal: a collection inside the measured window
  is charged to whichever benchmark happened to trigger it. Raising @-A@ usually
  removes it.
  -}
  , gcMajorCollections :: !Word32
  , gcCopiedBytes :: !Word64
  {- ^ Bytes copied by the collector. Sensitive to @-A@ and to how much garbage the
  previous benchmark left behind.
  -}
  , gcPeakMemInUseBytes :: !Word64
  {- ^ Peak address space in use.

  Not a delta: @max_mem_in_use_bytes@ is monotone, so subtracting endpoints
  yields ~0 and means nothing. This is the larger of the two snapshots.
  -}
  }
  deriving (Show, Eq)

{- | Whether @-T@ was given, i.e. whether 'measGC' will be populated.

Allocation is measured regardless.
-}
gcStatsAvailable :: IO Bool
gcStatsAvailable = getRTSStatsEnabled

{- | Run an action and report what it allocated.

The bracket is: major GC, snapshot, action, snapshot. The leading collection gives
the action a clean heap and keeps the previous benchmark's garbage off this one's
account. There is deliberately no trailing collection — see 'gcCollections'.

Compose this outside CodSpeed's measurement window, so the collections stay out of
the counted region:

@
(a, stats) <- 'measureRTS' $
  'CodSpeed.Instrument.benchmarkWith' opts{'CodSpeed.Instrument.optPerformGC' = False} sess uri act
@

Note @optPerformGC = False@: this function owns the collections, and doing them
twice merely wastes time under a 200x simulation slowdown.
-}
measureRTS :: IO a -> IO (a, Measurement)
measureRTS act = do
  withGC <- gcStatsAvailable
  -- Unconditionally, and before any snapshot: a clean heap for the action, and
  -- the previous benchmark's garbage collected on its own account rather than
  -- this one's.
  performGC
  if withGC then full else allocOnly
  where
    allocOnly = do
      before <- getAllocationCounter
      a <- act
      after <- getAllocationCounter
      pure (a, Measurement {measAllocatedBytes = countedDown before after, measGC = Nothing})

    full = do
      pre <- getRTSStats
      before <- getAllocationCounter
      a <- act
      after <- getAllocationCounter
      -- Deliberately no collection here. It would be charged to the region and
      -- make 'gcCollections' read at least 1 for every benchmark, destroying the
      -- signal that a GC landed *inside* the window. The allocation counter is
      -- byte-accurate without one.
      post <- getRTSStats
      pure
        ( a
        , Measurement
            { measAllocatedBytes = countedDown before after
            , measGC = Just (diffGC pre post)
            }
        )

{- | The allocation counter runs downwards, so the earlier reading is the larger
one. Clamped, because a thread that hits an allocation limit can have its counter
reset underneath us.
-}
countedDown :: Int64 -> Int64 -> Word64
countedDown before after
  | before >= after = fromIntegral (before - after)
  | otherwise = 0

diffGC :: RTSStats -> RTSStats -> GCDelta
diffGC pre post =
  GCDelta
    { gcCollections = gcs post - gcs pre
    , gcMajorCollections = major_gcs post - major_gcs pre
    , gcCopiedBytes = copied_bytes post - copied_bytes pre
    , gcPeakMemInUseBytes = max (max_mem_in_use_bytes post) (max_mem_in_use_bytes pre)
    }

{- | The allocation 'measureRTS' charges for its own bookkeeping.

Small — reading the counter costs a few words — but a benchmark run under
instrumentation executes its body exactly once, so there is no iteration count to
divide it away.
-}
newtype AllocationFloor = AllocationFloor {allocationFloorBytes :: Word64}
  deriving (Show, Eq, Ord)

{- | Measure the floor by running 'measureRTS' over an empty body, exercising
exactly the path a real measurement takes.

Takes the minimum of several runs: the first is inflated by one-time costs, and
the steady-state figure is the one worth subtracting.
-}
calibrateAllocationFloor :: IO AllocationFloor
calibrateAllocationFloor = calibrateAllocationFloorWith 5

-- | 'calibrateAllocationFloor' with an explicit repeat count (at least 1).
calibrateAllocationFloorWith :: Int -> IO AllocationFloor
calibrateAllocationFloorWith n = do
  samples <- mapM (const one) [1 .. max 1 n]
  pure . AllocationFloor $ minimum samples
  where
    one = do
      (_, m) <- measureRTS (pure ())
      pure (measAllocatedBytes m)

{- | Remove the bracket's own overhead from a measurement.

Saturates at zero: a benchmark cannot allocate a negative number of bytes, and
seeing one would mean the floor was calibrated under different conditions.

>>> measAllocatedBytes (subtractFloor (AllocationFloor 400) (Measurement 5000 Nothing))
4600

>>> measAllocatedBytes (subtractFloor (AllocationFloor 9000) (Measurement 100 Nothing))
0
-}
subtractFloor :: AllocationFloor -> Measurement -> Measurement
subtractFloor (AllocationFloor floorBytes) m =
  m {measAllocatedBytes = saturatingSub (measAllocatedBytes m) floorBytes}

-- | Subtraction that clamps at zero rather than wrapping. See 'subtractFloor'.
saturatingSub :: Word64 -> Word64 -> Word64
saturatingSub a b
  | a >= b = a - b
  | otherwise = 0

{- | A compact one-line summary, for console output and CI logs.

>>> renderMeasurement (Measurement 3201464 Nothing)
"3201464 B allocated"

>>> renderMeasurement (Measurement 3201464 (Just (GCDelta 2 0 8192 0)))
"3201464 B allocated, 8192 B copied, 2 GCs (0 major)"
-}
renderMeasurement :: Measurement -> String
renderMeasurement m =
  show (measAllocatedBytes m) <> " B allocated" <> maybe "" renderGC (measGC m)
  where
    renderGC g =
      ", "
        <> show (gcCopiedBytes g)
        <> " B copied, "
        <> show (gcCollections g)
        <> " GCs ("
        <> show (gcMajorCollections g)
        <> " major)"
