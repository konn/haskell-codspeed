{- | Checking that the RTS is configured for trustworthy measurement.

Getting this wrong is not loud. Nothing crashes, numbers still appear, and they
are simply about something other than the code under test. The motivating case:
a benchmark suite linked with @-with-rtsopts=-A32m -T --nonmoving-gc@ reported
that 76% of its measurement was @performMajorGC@ — the concurrent collector
marking a large live heap — and roughly 1-2% was the algorithm being benchmarked.
Every number was reproducible, plausible, and meaningless.

So the checks run at session start and say so on stderr.

'preflight' is deliberately a thin wrapper over the pure 'preflightPure', which
takes a snapshot record rather than reading the RTS. That keeps every rule
testable on any machine, including ones where simulation mode cannot run.
-}
module CodSpeed.RTS.Preflight (
  -- * Rules
  Severity (..),
  Warning (..),
  preflightPure,

  -- ** The mode the rules are relative to
  Mode (..),

  -- * Running them
  RTSSnapshot (..),
  defaultSnapshot,
  takeSnapshot,
  preflight,
  renderWarning,
) where

import CodSpeed.Instrument (Mode (..))
import CodSpeed.Instrument.Raw (c_nonmovingGC)
import Control.Monad (unless)
import Data.Word (Word32, Word64)
import GHC.RTS.Flags (
  GCFlags (..),
  MiscFlags (..),
  ParFlags (..),
  getGCFlags,
  getMiscFlags,
  getParFlags,
 )
import GHC.Stats (getRTSStatsEnabled)
import System.IO (hPutStrLn, stderr)

-- | How much a finding matters.
data Severity
  = -- | The measurement will be about something other than the code under test.
    Critical
  | -- | Worth fixing; the measurement is still usable.
    Advisory
  deriving (Show, Eq, Ord)

data Warning = Warning
  { warningSeverity :: !Severity
  , warningSummary :: !String
  -- ^ What is wrong.
  , warningRemedy :: !String
  -- ^ What to change, concretely enough to paste.
  }
  deriving (Show, Eq)

{- | The RTS settings the rules care about.

A plain record so 'preflightPure' can be exercised over configurations the host
machine is not actually in.
-}
data RTSSnapshot = RTSSnapshot
  { snapNonmovingGC :: !Bool
  -- ^ @--nonmoving-gc@.
  , snapCapabilities :: !Word32
  -- ^ @-N@.
  , snapParGcEnabled :: !Bool
  -- ^ Parallel GC, i.e. not @-qg@.
  , snapAllocAreaBlocks :: !Word32
  -- ^ @-A@, in 4096-byte blocks. @-A32m@ is 8192.
  , snapTickIntervalNs :: !Word64
  -- ^ The RTS timer. Zero means @-V0@.
  , snapIdleGC :: !Bool
  -- ^ @-I@ non-zero.
  , snapStatsEnabled :: !Bool
  -- ^ @-T@.
  }
  deriving (Show, Eq)

{- | A configuration that passes every rule: what @-A32m -T -V0 -I0 -N1@ with the
copying collector looks like.

Chiefly a base for building test cases and doctests by record update.

>>> preflightPure Simulation defaultSnapshot
[]
-}
defaultSnapshot :: RTSSnapshot
defaultSnapshot =
  RTSSnapshot
    { snapNonmovingGC = False
    , snapCapabilities = 1
    , snapParGcEnabled = False
    , snapAllocAreaBlocks = 8192 -- -A32m
    , snapTickIntervalNs = 0 -- -V0
    , snapIdleGC = False -- -I0
    , snapStatsEnabled = True -- -T
    }

-- | Bytes per block in @-A@ accounting. Verified against @-A32m@ giving 8192.
blockSize :: Word64
blockSize = 4096

-- | Below this, a GC inside the measured window becomes likely.
recommendedAllocAreaBytes :: Word64
recommendedAllocAreaBytes = 32 * 1024 * 1024

{- | Every rule, as a pure function of the mode and the RTS settings.

Returns 'Critical' findings first.

Nothing is reported when not running under CodSpeed — the settings only matter
for a measured run, and warning during ordinary development would be noise:

>>> preflightPure NotInstrumented (defaultSnapshot { snapNonmovingGC = True })
[]

The nonmoving collector is the one that silently dominates a measurement:

>>> map warningSeverity (preflightPure Simulation defaultSnapshot { snapNonmovingGC = True })
[Critical]
-}
preflightPure :: Mode -> RTSSnapshot -> [Warning]
preflightPure mode snap
  | mode == NotInstrumented = []
  | otherwise = critical <> advisory
  where
    critical = nonmoving <> parallel
    advisory = allocArea <> ticker <> idleGC <> stats

    simulating = mode == Simulation || isUnknown mode
    isUnknown m = case m of UnknownMode _ -> True; _ -> False

    nonmoving =
      [ Warning
          Critical
          "--nonmoving-gc is on: the concurrent collector marks on its own thread, \
          \so its cost lands nondeterministically inside benchmark windows"
          "Drop --nonmoving-gc from -with-rtsopts for CodSpeed runs."
      | snapNonmovingGC snap
      ]

    -- Parallel GC divides work across capabilities by whatever is idle, so the
    -- instruction count stops being a function of the program alone.
    parallel =
      [ Warning
          Critical
          ( "-N"
              <> show (snapCapabilities snap)
              <> " with parallel GC: collection work is split nondeterministically \
                 \across capabilities"
          )
          "Use -N1, or add -qg to disable parallel GC. Benchmarks must also run \
          \sequentially (-j1)."
      | snapCapabilities snap > 1
      , snapParGcEnabled snap
      ]

    allocArea =
      [ Warning
          Advisory
          ( "-A is "
              <> show (allocBytes `div` 1024)
              <> " kB; a collection inside the measured window is charged to \
                 \whichever benchmark triggered it"
          )
          "Raise it, e.g. -A32m, and check the reported GC count is zero."
      | allocBytes < recommendedAllocAreaBytes
      ]
    allocBytes = fromIntegral (snapAllocAreaBlocks snap) * blockSize

    -- Under simulation everything runs ~200x slower, so the timer fires
    -- enormously more often relative to the work being measured.
    ticker =
      [ Warning
          Advisory
          "The RTS timer is running; under CPU simulation it fires far more often \
          \relative to simulated work, adding context switches"
          "Add -V0."
      | simulating
      , snapTickIntervalNs snap /= 0
      ]

    idleGC =
      [ Warning
          Advisory
          "Idle GC is on, so a collection can land in a window purely because the \
          \process paused"
          "Add -I0."
      | snapIdleGC snap
      ]

    stats =
      [ Warning
          Advisory
          "-T is off, so per-benchmark GC statistics cannot be collected"
          "Add -T to -with-rtsopts. Allocation is still measured without it."
      | not (snapStatsEnabled snap)
      ]

-- | Read the current settings.
takeSnapshot :: IO RTSSnapshot
takeSnapshot = do
  gc <- getGCFlags
  par <- getParFlags
  misc <- getMiscFlags
  nonmoving <- c_nonmovingGC
  statsOn <- getRTSStatsEnabled
  pure
    RTSSnapshot
      { snapNonmovingGC = nonmoving
      , snapCapabilities = nCapabilities par
      , snapParGcEnabled = parGcEnabled par
      , snapAllocAreaBlocks = minAllocAreaSize gc
      , snapTickIntervalNs = tickInterval misc
      , snapIdleGC = doIdleGC gc
      , snapStatsEnabled = statsOn
      }

{- | Run the checks and report anything found on stderr.

Returns the findings so a caller can escalate; nothing here aborts a run, because
a wrong flag is worth shouting about but not worth failing CI over on its own.
-}
preflight :: Mode -> IO [Warning]
preflight mode = do
  ws <- preflightPure mode <$> takeSnapshot
  mapM_ (hPutStrLn stderr . renderWarning) ws
  unless (null ws) $
    hPutStrLn stderr "[codspeed] see CodSpeed.RTS.Preflight for why these matter"
  pure ws

{- | One line per finding.

>>> putStrLn (renderWarning (Warning Advisory "-T is off" "Add -T."))
[codspeed] advisory: -T is off
[codspeed]   fix: Add -T.
-}
renderWarning :: Warning -> String
renderWarning w =
  "[codspeed] "
    <> label
    <> ": "
    <> warningSummary w
    <> "\n[codspeed]   fix: "
    <> warningRemedy w
  where
    label = case warningSeverity w of
      Critical -> "CRITICAL"
      Advisory -> "advisory"
