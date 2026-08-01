{- | The rules are pure, so every configuration can be exercised regardless of
what the host machine's RTS is actually set to.

The @--nonmoving-gc@ case is the one with a real scar behind it: a suite carrying
it in @-with-rtsopts@ spent 76% of its measured cost inside @performMajorGC@ and
about 1-2% on the code being benchmarked.
-}
module PreflightSpec (
  test_quietWhenNotInstrumented,
  test_criticalRules,
  test_advisoryRules,
  test_liveSnapshot,
) where

import CodSpeed.RTS.Preflight
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

severities :: Mode -> RTSSnapshot -> [Severity]
severities m = map warningSeverity . preflightPure m

test_quietWhenNotInstrumented :: TestTree
test_quietWhenNotInstrumented =
  testGroup
    "silent outside CodSpeed"
    [ testCase "a clean configuration produces nothing" $
        preflightPure Simulation defaultSnapshot @?= []
    , -- These settings are perfectly reasonable for ordinary development, so
      -- complaining about them outside a measured run would just be noise.
      testCase "even a hostile configuration is ignored when not instrumented" $
        preflightPure
          NotInstrumented
          defaultSnapshot
            { snapNonmovingGC = True
            , snapCapabilities = 8
            , snapParGcEnabled = True
            , snapAllocAreaBlocks = 256
            }
          @?= []
    ]

test_criticalRules :: TestTree
test_criticalRules =
  testGroup
    "critical"
    [ testCase "--nonmoving-gc is flagged" $
        severities Simulation defaultSnapshot {snapNonmovingGC = True} @?= [Critical]
    , testCase "parallel GC across several capabilities is flagged" $
        severities
          Simulation
          defaultSnapshot {snapCapabilities = 8, snapParGcEnabled = True}
          @?= [Critical]
    , -- -qg makes multiple capabilities tolerable: collection stops being split
      -- nondeterministically across them.
      testCase "several capabilities with -qg is not flagged" $
        severities
          Simulation
          defaultSnapshot {snapCapabilities = 8, snapParGcEnabled = False}
          @?= []
    , testCase "-N1 with parallel GC enabled is not flagged" $
        severities
          Simulation
          defaultSnapshot {snapCapabilities = 1, snapParGcEnabled = True}
          @?= []
    , testCase "critical findings are ordered before advisory ones" $
        severities
          Simulation
          defaultSnapshot {snapNonmovingGC = True, snapStatsEnabled = False}
          @?= [Critical, Advisory]
    , testCase "the remedy is specific enough to act on" $
        case preflightPure Simulation defaultSnapshot {snapNonmovingGC = True} of
          [w] ->
            assertBool
              ("remedy should name the flag, got: " <> warningRemedy w)
              ("--nonmoving-gc" `substringOf` warningRemedy w)
          ws -> assertBool ("expected exactly one warning, got " <> show (length ws)) False
    ]

test_advisoryRules :: TestTree
test_advisoryRules =
  testGroup
    "advisory"
    [ testCase "a small -A is flagged" $
        severities Simulation defaultSnapshot {snapAllocAreaBlocks = 1024} @?= [Advisory]
    , testCase "-A32m is accepted" $
        severities Simulation defaultSnapshot {snapAllocAreaBlocks = 8192} @?= []
    , testCase "a running timer is flagged under simulation" $
        severities Simulation defaultSnapshot {snapTickIntervalNs = 10000000} @?= [Advisory]
    , -- Walltime mode wants the timer; only simulation is distorted by it.
      testCase "a running timer is accepted under walltime" $
        severities Walltime defaultSnapshot {snapTickIntervalNs = 10000000} @?= []
    , testCase "idle GC is flagged" $
        severities Simulation defaultSnapshot {snapIdleGC = True} @?= [Advisory]
    , testCase "missing -T is flagged" $
        severities Simulation defaultSnapshot {snapStatsEnabled = False} @?= [Advisory]
    , -- An unrecognised mode is treated as simulation: warning unnecessarily is
      -- better than staying quiet about a distorted measurement.
      testCase "an unknown mode is treated as simulation" $
        severities (UnknownMode "future") defaultSnapshot {snapTickIntervalNs = 10000000}
          @?= [Advisory]
    ]

test_liveSnapshot :: TestTree
test_liveSnapshot =
  testGroup
    "takeSnapshot"
    [ testCase "reads the real RTS without throwing" $ do
        s <- takeSnapshot
        assertBool "expected at least one capability" (snapCapabilities s >= 1)
    , testCase "sees the -T this suite is linked with" $ do
        s <- takeSnapshot
        snapStatsEnabled s @?= True
    , -- Reads RtsFlags.GcFlags.useNonmoving through the C shim, since
      -- GHC.RTS.Flags does not expose it.
      testCase "reports the nonmoving collector as off for this suite" $ do
        s <- takeSnapshot
        snapNonmovingGC s @?= False
    ]

substringOf :: String -> String -> Bool
substringOf needle haystack =
  any (startsWith needle) (tails' haystack)
  where
    startsWith [] _ = True
    startsWith _ [] = False
    startsWith (a : as) (b : bs) = a == b && startsWith as bs
    tails' [] = [[]]
    tails' s@(_ : rest) = s : tails' rest
