{-# LANGUAGE ScopedTypeVariables #-}

{- | A drop-in @tasty-bench@ runner that reports each benchmark to CodSpeed.

Swap the import and nothing else:

@
-- import Test.Tasty.Bench
import Test.Tasty.Bench.CodSpeed

main :: IO ()
main = 'defaultMain'
  [ bgroup "solve" [ bench "uf20" (nf solve cnf) ] ]
@

Everything @Test.Tasty.Bench@ exports is re-exported unchanged; only
'defaultMain' differs.

== What changes under CodSpeed

Outside the CodSpeed runner this behaves like @tasty-bench@ — same adaptive timing
loop, same output, same ingredients. Under it, every @bench@ leaf becomes its own
CodSpeed benchmark: the body runs __exactly once__, inside a tight measurement
window, reported under a URI derived from the tasty path.

That per-leaf window is the whole point. Measuring a whole process instead — which
is what the zero-code-change @codspeed.yml@ route does — sweeps in RTS startup,
benchmark-tree construction, input loading and console output. On a real suite
that dominated completely: 76% of one measured target was @performMajorGC@ and
roughly 1-2% was the code under test.

== One iteration, and what that demands of your benchmarks

Under instrumentation the body is evaluated once, so nothing is averaged and no
first noisy run is discarded. Two consequences:

* @whnf f x@ shares the thunk for @x@ across iterations, and @tasty-bench@ relies
  on a first run to force it. With one run there is no such run to throw away, and
  forcing @x@ happens inside the measurement. Force inputs beforehand — @env@
  already does, since tasty evaluates resources to normal form before the test
  action runs.
* @nfIO (pure x)@ lets GHC float @x@ out and evaluate it once, leaving the window
  measuring @pure@. Prefer @nfAppIO@.
* Beware fusion. @nf (sum . enumFromTo 1) n@ compiles at @-O2@ to an unboxed loop
  that allocates nothing; a benchmark can easily measure less than it looks.

== No default timeout under instrumentation

@tasty-bench@'s @defaultMain@ imposes a 100-second per-benchmark timeout. Under
the CPU-simulation instrument everything runs roughly 200x slower, so that
threshold is reached by work that takes half a second natively. Worse, the failure
is quiet: the benchmark is cut short and the instrument still reports the
instruction count of the truncated run, and because the cap is wall-clock the set
of victims shifts with machine load.

So this runner installs no default timeout when instrumented. An explicit
@localOption (mkTimeout n)@ is still honoured — if you set one, make sure it
accounts for the slowdown.

== Results still flow to tasty-bench's reporters

@--csv@, @--baseline@, @--svg@ and @bcompare@ keep working: results are encoded in
@tasty-bench@'s own format (see "Test.Tasty.Bench.CodSpeed.Internal"). The
allocation figure there is exact and deterministic. The /time/ figure is real
elapsed time, which under simulation is inflated by Valgrind — CodSpeed's own
number is the metric, not that one.
-}
module Test.Tasty.Bench.CodSpeed (
  -- * Running benchmarks
  defaultMain,
  defaultMainWith,

  -- * Configuration
  Config (..),
  defaultConfig,

  -- * Everything else from tasty-bench
  module Test.Tasty.Bench,
) where

import CodSpeed.Instrument (
  Integration (..),
  Session,
  isInstrumented,
  sessionMode,
  withSession,
 )
import CodSpeed.Instrument qualified as CS
import CodSpeed.RTS.Eventlog qualified as EL
import CodSpeed.RTS.Preflight (preflight)
import CodSpeed.RTS.Stats (
  gcCopiedBytes,
  gcPeakMemInUseBytes,
  measAllocatedBytes,
  measGC,
  measureRTS,
 )
import Control.Exception (bracket_)
import Control.Monad.Trans.Cont (ContT, runContT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Tagged (Tagged, retag)
import Data.Typeable (cast)
import Data.Version (showVersion)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getProgName, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO.Unsafe (unsafePerformIO)
import System.Info (fullCompilerVersion)
import Test.Tasty (Timeout (..), adjustOption, mkTimeout, testGroup)
import Test.Tasty.Bench hiding (defaultMain)
import Test.Tasty.Bench.CodSpeed.Internal (
  Estimate (..),
  Measurement (..),
  encodeResult,
 )
import Test.Tasty.Ingredients.ConsoleReporter (MinDurationToReport (..))
import Test.Tasty.Options (OptionDescription, OptionSet, lookupOption, setOption)
import Test.Tasty.Providers (IsTest (..), Result, testFailed, testPassed)
import Test.Tasty.Runners (
  NumThreads (..),
  TestTree (..),
  installSignalHandlers,
  parseOptions,
  tryIngredients,
 )

-- | How the runner identifies itself and names benchmarks.
data Config = Config
  { configIntegration :: Integration
  -- ^ Reported to CodSpeed as the producer of these results.
  , configSourcePath :: Maybe FilePath
  {- ^ The leading component of every benchmark URI.

  CodSpeed's convention is @{git-relative-path}::{name}@. Haskell has no
  equivalent of Rust's @file!()@, so when this is 'Nothing' the runner falls
  back to @$CODSPEED_HS_SOURCE_FILE@ and then to the executable name. Non-path
  URIs are accepted — CodSpeed's own exec harness emits @exec_harness::\<name\>@
  — but a real path makes results easier to navigate.
  -}
  , configRootFrame :: Bool
  {- ^ Wrap each body in a @__codspeed_root_frame__@ C frame.

  Off by default; see "CodSpeed.Instrument.RootFrame" for what it costs.
  -}
  }

-- | This package's identity, no source path, no root frame.
defaultConfig :: Config
defaultConfig =
  Config
    { configIntegration =
        Integration
          { integrationName = "haskell-codspeed"
          , integrationVersion = "0.1.0.0"
          }
    , configSourcePath = Nothing
    , configRootFrame = False
    }

{- | Per-process runner state.

Global because a tasty test tree cannot carry it: 'IsTest' instances receive only
an 'OptionSet', and neither a 'Session' nor a config is a sensible tasty option.
One session per process is the right shape anyway, and 'defaultMainWith' owns its
lifetime.
-}
data RunnerState = RunnerState
  { stateSession :: !Session
  , stateRootFrame :: !Bool
  , stateEventlog :: !Bool
  {- ^ Resolved once, because 'CodSpeed.RTS.Eventlog.eventlogEnabled' reads RTS
  flags and markers cost heap allocation even when the eventlog is off.
  -}
  }

runnerRef :: IORef (Maybe RunnerState)
runnerRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE runnerRef #-}

-- | A benchmark leaf, rewritten to report itself.
data CodSpeedBench = CodSpeedBench !String !Payload

{- | @benchCont@ yields a @ContT () IO Benchmarkable@ rather than a
'Benchmarkable', and a rewrite recognising only the latter would silently skip
those leaves.
-}
data Payload
  = Direct Benchmarkable
  | Cont (ContT () IO Benchmarkable)

instance IsTest CodSpeedBench where
  -- Inherit tasty-bench's own options (RelStDev, FailIfSlower, FailIfFaster,
  -- TimeMode) so existing flags keep working on the fallback path.
  testOptions = retag (testOptions :: Tagged Benchmarkable [OptionDescription])

  run opts (CodSpeedBench uri payload) yieldProgress = do
    mstate <- readIORef runnerRef
    -- Markers wrap both paths deliberately. Slicing an eventlog per benchmark is
    -- most useful when running locally with +RTS -l, which is exactly when
    -- CodSpeed is *not* attached; emitting them only under the runner would put
    -- the feature out of reach in its main use case. On the fallback path one
    -- region covers tasty-bench's whole adaptive loop, which is what you want.
    EL.withRegion (maybe False stateEventlog mstate) uri $ case mstate of
      Just st
        | isInstrumented (stateSession st) ->
            case lookupOption opts of
              NumThreads 1 -> measurePayload st opts uri payload
              _ ->
                pure . testFailed $
                  "Benchmarks must not run concurrently under CodSpeed: the "
                    <> "measurement window would span whatever else is running, and "
                    <> "allocation is charged to whichever measurement is open. "
                    <> "Pass -j1 and avoid +RTS -N."
      -- No runner attached: hand straight back to tasty-bench, which keeps its
      -- adaptive timing loop and every other behaviour.
      _ -> case payload of
        Direct b -> run opts b yieldProgress
        Cont c -> run opts c yieldProgress

measurePayload :: RunnerState -> OptionSet -> String -> Payload -> IO Result
measurePayload st opts uri payload = case payload of
  Direct b -> measureOne st opts uri b
  Cont c -> do
    -- runContT's continuation must return IO (), so the Result comes back
    -- through an IORef rather than as its value.
    slot <- newIORef Nothing
    runContT c $ \b -> measureOne st opts uri b >>= writeIORef slot . Just
    fromMaybe
      (testFailed "benchCont's runner never invoked its continuation")
      <$> readIORef slot

measureOne :: RunnerState -> OptionSet -> String -> Benchmarkable -> IO Result
measureOne st opts uri b = do
  t0 <- getMonotonicTimeNSec
  -- No eventlog markers here: they are emitted by `run`, outside this bracket,
  -- because traceEventIO allocates on the GHC heap even with the eventlog off
  -- and one iteration gives no repeat count to divide that away.
  --
  -- measureRTS owns the GC bracket, so the instrument must not add its own.
  (_, stats) <-
    measureRTS $
      CS.benchmarkWith
        CS.defaultOptions
          { CS.optPerformGC = False
          , CS.optRootFrame = stateRootFrame st
          }
        (stateSession st)
        uri
        (unBenchmarkable b 1)
  t1 <- getMonotonicTimeNSec
  let gc = measGC stats
      est =
        Estimate
          { estMean =
              Measurement
                { measTime = (t1 - t0) * 1000 -- ns -> ps
                , measAllocs = measAllocatedBytes stats
                , measCopied = maybe 0 gcCopiedBytes gc
                , measMaxMem = maybe 0 gcPeakMemInUseBytes gc
                }
          , -- One iteration, so there is no spread. tasty-bench renders a zero
            -- stdev by dropping the +/- column, which is the honest display.
            estStdev = 0
          }
      FailIfSlower ifSlower = lookupOption opts
      FailIfFaster ifFaster = lookupOption opts
  pure . testPassed $ encodeResult est (1 - ifFaster) (1 + ifSlower)

{- | Rewrite every benchmark leaf so it reports itself.

'mapLeafBenchmarks' supplies the path with the leaf's own name first, so it is
reversed to read outermost-group-first. Leaves whose payload is neither a
'Benchmarkable' nor a @ContT () IO Benchmarkable@ — an ordinary tasty test mixed
into the tree — are left untouched.
-}
instrumentTree :: String -> TestTree -> TestTree
instrumentTree prefix = mapLeafBenchmarks $ \path tree -> case tree of
  SingleTest name t ->
    -- drop 1 removes the synthetic "All" root that tasty-bench's defaultMain
    -- wraps every suite in. It is the same for every benchmark, so keeping it
    -- would put a constant component in every URI for no benefit -- and URIs are
    -- the identity CodSpeed tracks across runs, so they should carry only what
    -- distinguishes one benchmark from another.
    let uri = mkUri prefix (drop 1 (reverse path))
     in case cast t of
          Just b -> SingleTest name (CodSpeedBench uri (Direct b))
          Nothing -> case cast t of
            Just c -> SingleTest name (CodSpeedBench uri (Cont c))
            Nothing -> tree
  other -> other

{- | @prefix::group::group::name@.

@::@ is CodSpeed's separator, so any occurring inside a benchmark name is
rewritten to keep the URI unambiguous.
-}
mkUri :: String -> [String] -> String
mkUri prefix path = intercalate "::" (prefix : map sanitise path)
  where
    sanitise (':' : ':' : rest) = '_' : '_' : sanitise rest
    sanitise (c : rest) = c : sanitise rest
    sanitise [] = []

-- | 'defaultMainWith' at 'defaultConfig'.
defaultMain :: [Benchmark] -> IO ()
defaultMain = defaultMainWith defaultConfig

{- | Run a benchmark tree, reporting to CodSpeed when a runner is present.

Opens a session, warns about RTS settings that would distort the measurement (see
"CodSpeed.RTS.Preflight"), rewrites the tree and hands off to tasty.

Safe to call unconditionally: with no runner attached this is @tasty-bench@'s
@defaultMain@, including its 100-second default timeout. That timeout is omitted
only when instrumented, for the reason given in the module header.
-}
defaultMainWith :: Config -> [Benchmark] -> IO ()
defaultMainWith cfg benchmarks =
  withSession (configIntegration cfg) $ \sess -> do
    _ <- preflight (sessionMode sess)
    prefix <- resolvePrefix (configSourcePath cfg)
    reportBuildEnvironment sess
    installSignalHandlers

    let instrumented = isInstrumented sess
        tree0 = instrumentTree prefix (testGroup "All" benchmarks)
        tree = if instrumented then tree0 else withDefaultTimeout tree0

    opts <- parseOptions benchIngredients tree
    -- Both mirror tasty-bench: benchmarks never run concurrently, and the
    -- console reporter should not editorialise about short runs.
    let opts' =
          setOption (NumThreads 1) $
            setOption (MinDurationToReport 1000000000000) opts

    evOn <- EL.eventlogEnabled
    bracket_
      (writeIORef runnerRef (Just (RunnerState sess (configRootFrame cfg) evOn)))
      (writeIORef runnerRef Nothing)
      ( case tryIngredients benchIngredients opts' tree of
          Nothing -> exitFailure
          Just act -> act >>= \ok -> if ok then exitSuccess else exitFailure
      )

{- | @tasty-bench@'s default 100-second per-benchmark cap, applied only when not
instrumented. An explicit timeout always wins.
-}
withDefaultTimeout :: TestTree -> TestTree
withDefaultTimeout = adjustOption $ \opt -> case opt of
  Timeout {} -> opt
  NoTimeout -> mkTimeout 100000000

-- | @configSourcePath@, else @$CODSPEED_HS_SOURCE_FILE@, else the program name.
resolvePrefix :: Maybe FilePath -> IO String
resolvePrefix (Just p) = pure p
resolvePrefix Nothing = do
  fromEnv <- lookupEnv "CODSPEED_HS_SOURCE_FILE"
  case fromEnv of
    Just p | not (null p) -> pure p
    _ -> getProgName

{- | Record what would silently invalidate a baseline.

A change of GHC version, RTS flags or @-A@ shifts instruction counts exactly as a
code change does, so it belongs in the run's metadata rather than in someone's
memory.
-}
reportBuildEnvironment :: Session -> IO ()
reportBuildEnvironment sess = do
  CS.reportEnvironment
    sess
    "haskell"
    [ ("ghc-version", showVersion fullCompilerVersion)
    , ("instrument-hooks", CS.instrumentHooksCommit)
    ]
  CS.writeEnvironment sess
