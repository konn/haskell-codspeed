{- | The determinism assertions here are the ones that justify treating allocation
as a metric worth gating CI on, so they are exact rather than tolerant.

They pass under a concurrently-running test suite only because
'CodSpeed.RTS.Stats.measureRTS' reads a per-thread counter. An earlier version
built on @GHC.Stats.allocated_bytes@ failed them, because that counter is
process-wide.
-}
module StatsSpec (
  test_gcStatsAvailable,
  test_allocationIsObserved,
  test_allocationAcrossThreads,
  test_allocationFloor,
  test_subtractFloor,
) where

import CodSpeed.Instrument.RootFrame (withRootFrame)
import CodSpeed.RTS.Stats
import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.List (nub)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

test_gcStatsAvailable :: TestTree
test_gcStatsAvailable =
  testCase "the suite runs with -T, so GC stats are populated too" $
    gcStatsAvailable >>= assertBool "expected -T; check -with-rtsopts in the cabal file"

{- | Allocates roughly @24 * n@ bytes of cons cells.

'reverse' is used rather than a bare @sum [1 .. n]@ because the latter fuses at
@-O2@ into an unboxed accumulator loop that allocates nothing at all — which is
worth knowing, since it is exactly the trap a naive Haskell benchmark falls into.
'reverse' is not a good producer, so the list is genuinely materialised.
-}
{-# NOINLINE allocateSome #-}
allocateSome :: Int -> IO Int
allocateSome n = evaluate (sum (reverse [1 .. n]))

test_allocationIsObserved :: TestTree
test_allocationIsObserved =
  testGroup
    "measureRTS"
    [ testCase "returns the action's result" $ do
        (r, _) <- measureRTS (allocateSome 1000)
        r @?= sum [1 .. 1000 :: Int]
    , testCase "sees allocation from an allocating action" $ do
        (_, m) <- measureRTS (allocateSome 200000)
        assertBool
          ("expected megabytes of cons cells, got " <> renderMeasurement m)
          (measAllocatedBytes m > 1000000)
    , -- The entire case for treating allocation as a co-equal metric rests on
      -- this. If it does not hold exactly, the metric is not worth gating on.
      testCase "allocation is exactly reproducible across runs" $ do
        deltas <- forM [1 .. 5 :: Int] $ \_ -> do
          (_, m) <- measureRTS (allocateSome 50000)
          pure (measAllocatedBytes m)
        assertBool
          ("expected identical allocation across runs, got " <> show deltas)
          (length (nub deltas) == 1)
    , testCase "allocation scales with the work done" $ do
        (_, small) <- measureRTS (allocateSome 10000)
        (_, big) <- measureRTS (allocateSome 100000)
        assertBool
          ( "10x the work should allocate substantially more: "
              <> renderMeasurement small
              <> " vs "
              <> renderMeasurement big
          )
          (measAllocatedBytes big > 5 * measAllocatedBytes small)
    , testCase "a trivial action allocates almost nothing" $ do
        (_, m) <- measureRTS (pure ())
        assertBool
          ("a no-op should stay in the hundreds of bytes, got " <> renderMeasurement m)
          (measAllocatedBytes m < 4096)
    , testCase "GC stats are populated under -T" $ do
        (_, m) <- measureRTS (allocateSome 100000)
        case measGC m of
          Nothing -> assertBool "expected GC stats under -T" False
          Just _ -> pure ()
    ]

{- | The bug this pins down shipped, and it was invisible in every local run.

'measureAllocation' reads a per-thread counter. 'withRootFrame' re-enters the RTS,
so the action runs on a fresh bound thread — and with the bracket left on the
outside, the measurement is of the calling thread, which by then is doing nothing.
Under CodSpeed every benchmark in the example suite reported 2.7–5.0 KB, lazy and
strict @fib@ alike, where natively they report 643418 B and 0 B.

Nothing failed. The suite passed, the CSV was written, the numbers were plausible
and constant.
-}
test_allocationAcrossThreads :: TestTree
test_allocationAcrossThreads =
  testGroup
    "measureAllocation and withRootFrame"
    [ testCase "sees the allocation when it is inside the root frame" $ do
        (_, n) <- withRootFrame (measureAllocation (allocateSome 200000))
        assertBool
          ("expected megabytes of cons cells, got " <> show n <> " B")
          (n > 1000000)
    , -- Not a wish, a warning: this is what the shipped code did, and this is
      -- what it reported. If this assertion ever starts failing because the
      -- outside sees the allocation too, the composition order stops mattering
      -- and the comment above should go.
      testCase "does not see it when it is outside" $ do
        (_, n) <- measureAllocation (withRootFrame (allocateSome 200000))
        assertBool
          ("expected the harness's own few kB, got " <> show n <> " B")
          (n < 100000)
    ]

test_allocationFloor :: TestTree
test_allocationFloor =
  testGroup
    "calibrateAllocationFloor"
    [ testCase "the floor is small -- bytes, not kilobytes" $ do
        AllocationFloor n <- calibrateAllocationFloor
        assertBool
          ("expected a floor under 4 kB, got " <> show n <> " B")
          (n < 4096)
    , -- Subtracting a floor calibrated once from measurements taken later is only
      -- sound if the floor is stable.
      testCase "the floor is stable across calibrations" $ do
        AllocationFloor a <- calibrateAllocationFloor
        AllocationFloor b <- calibrateAllocationFloor
        assertBool
          ("expected a stable floor, got " <> show a <> " then " <> show b)
          (a == b)
    ]

test_subtractFloor :: TestTree
test_subtractFloor =
  testGroup
    "subtractFloor"
    [ testCase "removes the bracket's own cost" $
        measAllocatedBytes (subtractFloor (AllocationFloor 400) (sample 5000)) @?= 4600
    , testCase "clamps at zero rather than wrapping" $
        measAllocatedBytes (subtractFloor (AllocationFloor 9000) (sample 100)) @?= 0
    , testCase "leaves the GC fields alone" $
        measGC (subtractFloor (AllocationFloor 400) (sample 5000)) @?= measGC (sample 5000)
    ]
  where
    sample n = Measurement {measAllocatedBytes = n, measGC = Just (GCDelta 2 1 8192 4096)}
