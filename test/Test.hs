module Main (main) where

import BenchEncodingSpec qualified
import CCSSpec qualified
import InstrumentSpec qualified
import PreflightSpec qualified
import SidecarSpec qualified
import StatsSpec qualified
import System.Environment (lookupEnv, setEnv)
import Test.Tasty (defaultMain, testGroup)

{- | Runs single-threaded by default.

@GHC.Stats.allocated_bytes@ is a process-wide counter, so anything else
allocating while a measurement is open lands inside that measurement's delta.
Left concurrent, the determinism assertions in "StatsSpec" fail against garbage —
observed as deltas of 76256, 63624, 51264, 3776, 3032 bytes for five runs of an
identical action, decreasing as the rest of the suite drained.

This is not a testing artifact but the real constraint on the metric, and it is
why @tasty-bench@ refuses outright to run when @numThreads \/= 1@. Rather than
weaken the assertions to accommodate a condition no real benchmark run is under,
the suite adopts the same rule.

@TASTY_NUM_THREADS@ is still honoured if set explicitly, so @-j2@ remains
available for debugging.
-}
main :: IO ()
main = do
  existing <- lookupEnv "TASTY_NUM_THREADS"
  case existing of
    Nothing -> setEnv "TASTY_NUM_THREADS" "1"
    Just _ -> pure ()
  defaultMain $
    testGroup
      "haskell-codspeed"
      [ testGroup
          "InstrumentSpec"
          [ InstrumentSpec.test_parseMode
          , InstrumentSpec.test_sessionWithoutRunner
          , InstrumentSpec.test_rootFrame
          ]
      , testGroup
          "StatsSpec"
          [ StatsSpec.test_gcStatsAvailable
          , StatsSpec.test_allocationIsObserved
          , StatsSpec.test_allocationAcrossThreads
          , StatsSpec.test_allocationFloor
          , StatsSpec.test_subtractFloor
          ]
      , testGroup
          "PreflightSpec"
          [ PreflightSpec.test_quietWhenNotInstrumented
          , PreflightSpec.test_criticalRules
          , PreflightSpec.test_advisoryRules
          , PreflightSpec.test_liveSnapshot
          ]
      , testGroup
          "BenchEncodingSpec"
          [ BenchEncodingSpec.test_encoding
          , BenchEncodingSpec.test_roundTrip
          ]
      , testGroup
          "SidecarSpec"
          [ SidecarSpec.test_csvRoundTrip
          , SidecarSpec.test_csvParsing
          , SidecarSpec.test_comparison
          ]
      , testGroup
          "CCSSpec"
          [ CCSSpec.test_unavailableWithoutProf
          , CCSSpec.test_findById
          , CCSSpec.test_diff
          , CCSSpec.test_pruning
          , CCSSpec.test_folded
          , CCSSpec.test_fileNames
          ]
      ]
