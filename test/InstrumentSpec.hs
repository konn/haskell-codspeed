{- | Everything here runs outside the CodSpeed runner, which is the state a
developer's machine is normally in — and, more importantly, the state a
benchmark suite must survive without complaint.
-}
module InstrumentSpec (
  test_parseMode,
  test_sessionWithoutRunner,
  test_rootFrame,
) where

import CodSpeed.Instrument
import CodSpeed.Instrument.RootFrame (withRootFrame)
import Control.Exception (ErrorCall (..), evaluate, throwIO, try)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

integration :: Integration
integration = Integration {integrationName = "haskell-codspeed-test", integrationVersion = "0"}

test_parseMode :: TestTree
test_parseMode =
  testGroup
    "parseMode"
    [ testCase "unset means not instrumented" $
        parseMode Nothing @?= NotInstrumented
    , testCase "empty means not instrumented" $
        parseMode (Just "") @?= NotInstrumented
    , -- The runner writes `instrumentation` on the wire while the CLI and docs
      -- both say `simulation`. Reading only one of them would silently mis-detect
      -- the mode in real runs.
      testCase "instrumentation and simulation agree" $
        parseMode (Just "instrumentation") @?= parseMode (Just "simulation")
    , testCase "simulation" $ parseMode (Just "simulation") @?= Simulation
    , testCase "walltime" $ parseMode (Just "walltime") @?= Walltime
    , testCase "memory" $ parseMode (Just "memory") @?= Memory
    , testCase "matching is case-insensitive" $
        parseMode (Just "WallTime") @?= Walltime
    , testCase "an unknown mode is preserved, not discarded" $
        parseMode (Just "warp-drive") @?= UnknownMode "warp-drive"
    ]

test_sessionWithoutRunner :: TestTree
test_sessionWithoutRunner =
  testGroup
    "session outside the runner"
    [ testCase "initialises and reports itself uninstrumented" $
        withSession integration $ \sess ->
          isInstrumented sess @?= False
    , testCase "reports a plausible pid" $
        withSession integration $ \sess ->
          assertBool "pid should be positive" (sessionPid sess > 0)
    , -- The whole point of degrading quietly: a suite written against this API
      -- has to be runnable directly, and the body must still execute.
      testCase "benchmark runs its action exactly once" $ do
        counter <- newIORef (0 :: Int)
        withSession integration $ \sess ->
          benchmark sess "test.hs::once" (modifyIORef' counter (+ 1))
        readIORef counter >>= (@?= 1)
    , testCase "reporting metadata is a no-op rather than an error" $
        withSession integration $ \sess -> do
          reportEnvironment sess "haskell" [("ghc", "9.12.4")]
          reportEnvironmentList sess "haskell" "rts-flags" ["-A32m", "-T"]
          writeEnvironment sess
    , testCase "an exception from the body is not swallowed" $ do
        r <- try . withSession integration $ \sess ->
          benchmark sess "test.hs::boom" (throwIO (ErrorCall "boom"))
        case r of
          Left (ErrorCall msg) -> msg @?= "boom"
          Right () -> fail "expected the exception to propagate"
    ]

{- | The root frame re-enters the RTS through a foreign export, so these exercise
@rts_lock@ and a fresh bound thread. The exception case is the one that matters:
an exception escaping an RTS in-call terminates the process rather than
propagating, so 'withRootFrame' has to catch and re-raise across the boundary.
-}
test_rootFrame :: TestTree
test_rootFrame =
  testGroup
    "withRootFrame"
    [ testCase "returns the action's result" $ do
        n <- withRootFrame (evaluate (sum [1 .. 100 :: Int]))
        n @?= 5050
    , testCase "runs effects exactly once" $ do
        counter <- newIORef (0 :: Int)
        withRootFrame (modifyIORef' counter (+ 1))
        readIORef counter >>= (@?= 1)
    , testCase "re-raises an exception instead of killing the process" $ do
        r <- try (withRootFrame (throwIO (ErrorCall "in-call boom")))
        case r of
          Left (ErrorCall msg) -> msg @?= "in-call boom"
          Right () -> fail "expected the exception to cross back out"
    , testCase "survives being nested" $ do
        n <- withRootFrame (withRootFrame (evaluate (21 * 2 :: Int)))
        n @?= 42
    ]
