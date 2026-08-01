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
import CodSpeed.Profiling.CCS qualified as CCS
import CodSpeed.RTS.Eventlog qualified as EL
import CodSpeed.RTS.Preflight (preflight)
import CodSpeed.RTS.Stats (
  gcCopiedBytes,
  gcPeakMemInUseBytes,
  measAllocatedBytes,
  measGC,
  measureRTS,
 )
import CodSpeed.Sidecar qualified as Sidecar
import Control.Exception (bracket_)
import Control.Monad (unless)
import Control.Monad.Trans.Cont (ContT, runContT)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Tagged (Tagged, retag)
import Data.Typeable (cast)
import Data.Version (showVersion)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getProgName, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Info (fullCompilerVersion)
import Test.Tasty (Timeout (..), adjustOption, mkTimeout, testGroup)
import Test.Tasty.Bench hiding (defaultMain)
import Test.Tasty.Bench.CodSpeed.Internal (
  Estimate (..),
  Measurement (..),
  WithLoHi (..),
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
  resultDescription,
  tryIngredients,
 )
import Text.Read (readMaybe)

-- | How the runner identifies itself and names benchmarks.
data Config = Config
  { configIntegration :: Integration
  -- ^ Reported to CodSpeed as the producer of these results.
  , configComponent :: Maybe String
  {- ^ The Cabal component the suite lives in, e.g. @example@.

  'Nothing' takes @$CODSPEED_HS_COMPONENT@, then the executable name — which for
  a @benchmark foo@ stanza is @foo@, since that is what Cabal names the binary.
  -}
  , configRootFrame :: Bool
  {- ^ Wrap each body in a @__codspeed_root_frame__@ C frame.

  __On by default.__ @CUSTOM_HARNESS.md@ says the benchmarked code must run
  inside such a frame, and it was initially read as a flamegraph nicety on the
  evidence that CodSpeed's Valgrind fork mentions the name only in comments about
  re-parenting. That reading looks wrong: without a root frame the backend
  recorded no benchmarks at all, from either this package or a C probe, while
  upstream's example — which has one — recorded fine through the identical
  pipeline.

  It is not free: see "CodSpeed.Instrument.RootFrame". The body runs on a fresh
  bound thread via an RTS in-call, which puts it out of reach of
  @System.Timeout.timeout@.
  -}
  , configSidecarPath :: Maybe FilePath
  {- ^ Where to write per-benchmark allocation, if anywhere.

  Falls back to @$CODSPEED_HS_SIDECAR@, and writes nothing when that is unset
  too. Independent of CodSpeed: allocation is exactly deterministic and is
  measurable on hosts where the simulation instrument cannot run. See
  "CodSpeed.Sidecar" and the @codspeed-hs-compare@ executable.
  -}
  , configCCSDir :: Maybe FilePath
  {- ^ Where to write per-benchmark cost-centre profiles, if anywhere.

  Falls back to @$CODSPEED_HS_CCS_DIR@. Only does anything in a @-prof@ build;
  see "CodSpeed.Profiling.CCS". Output is folded stacks, which @speedscope@ and
  @flamegraph.pl@ read directly — a Haskell-shaped flamegraph that needs nothing
  from CodSpeed.
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
    , configComponent = Nothing
    , configRootFrame = True
    , configSidecarPath = Nothing
    , configCCSDir = Nothing
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
  , stateSidecar :: !(IORef [Sidecar.Record])
  -- ^ Accumulated as benchmarks run, written once at the end.
  , stateCCSDir :: !(Maybe FilePath)
  -- ^ Where to drop per-benchmark folded stacks, in a profiling build.
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
    withCCS mstate uri $ EL.withRegion (maybe False stateEventlog mstate) uri $ case mstate of
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
      _ -> do
        r <- case payload of
          Direct b -> run opts b yieldProgress
          Cont c -> run opts c yieldProgress
        -- Still record allocation. tasty-bench measures it anyway and encodes it
        -- into resultDescription in exactly the format the mirror types read, so
        -- harvesting it costs nothing -- and the sidecar is most valuable
        -- precisely here, on hosts where the simulation instrument cannot run.
        mapM_ (\st -> harvestFallback st uri r) mstate
        pure r

{- | Pull allocation out of a result @tasty-bench@ produced.

Silently does nothing if the description does not parse — a suite can legitimately
contain leaves this package did not encode (a @bcompare@ node, say), and a
sidecar missing one row is better than a benchmark run that dies over
bookkeeping.
-}
harvestFallback :: RunnerState -> String -> Result -> IO ()
harvestFallback st uri r =
  case readMaybe (resultDescription r) of
    Nothing -> pure ()
    Just (WithLoHi est _ _) -> do
      let record =
            Sidecar.Record
              { Sidecar.recUri = uri
              , Sidecar.recAllocatedBytes = measAllocs (estMean est)
              , Sidecar.recCopiedBytes = measCopied (estMean est)
              , -- tasty-bench reports no collection counts, and a fabricated
                -- zero would be indistinguishable from a measured one.
                Sidecar.recCollections = 0
              , Sidecar.recMajorCollections = 0
              }
      modifyIORef' (stateSidecar st) (record :)

{- | Capture the cost centres a benchmark touched, in a profiling build.

The RTS has no way to reset cost-centre counters, so this snapshots the tree
either side and subtracts. That is expensive — proportional to the number of
distinct cost-centre stacks — but it only happens in a @-prof@ build, which is a
side-car run rather than the one whose numbers are reported.

A no-op in a vanilla build, or when no output directory was configured.
-}
withCCS :: Maybe RunnerState -> String -> IO a -> IO a
withCCS mstate uri act = case (CCS.ccsAvailable, mstate >>= stateCCSDir) of
  (True, Just dir) -> do
    -- Taken here rather than at the root, so the profile is re-rooted at this
    -- benchmark instead of at main. Trimming a shared prefix afterwards does not
    -- work: CAFs and the RTS's own pseudo-roots hang straight off MAIN, so there
    -- is nothing shared beyond MAIN itself.
    here <- CCS.currentCCSId
    before <- CCS.snapshotCCS
    r <- act
    after <- CCS.snapshotCCS
    case (before, after) of
      (Just b, Just a) -> do
        let diffed = CCS.diffCCS b a
            rooted = maybe Nothing (`CCS.findById` diffed) here
        CCS.writeFoldedStacks dir CCS.ByAllocation uri (fromMaybe diffed rooted)
      _ -> pure ()
    pure r
  _ -> act

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
  modifyIORef' (stateSidecar st) (Sidecar.fromMeasurement uri stats :)
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
instrumentTree component = mapLeafBenchmarks $ \path tree -> case tree of
  SingleTest name t ->
    -- The "All" root that tasty-bench wraps every suite in is kept: it is part of
    -- the identifier tasty-bench itself prints, and matching that exactly is what
    -- lets a sidecar row and a --csv row refer to the same benchmark.
    let uri = mkUri component (reverse path)
     in case cast t of
          Just b -> SingleTest name (CodSpeedBench uri (Direct b))
          Nothing -> case cast t of
            Just c -> SingleTest name (CodSpeedBench uri (Cont c))
            Nothing -> tree
  other -> other

{- | @component:tasty-bench-identifier@.

The Cabal component, then the benchmark's identifier exactly as @tasty-bench@
spells it — the dot-separated tasty path, @All@ root included. Matching it is the
point: @tasty-bench@'s own @--csv@ @Name@ column reads @All.fib.15@, so a sidecar
row and a @--csv@ row name the same benchmark the same way.

CodSpeed documents @{git-relative-path}::{name}@, but its backend accepts either —
a probe run confirmed a bare name, a @::@-separated name and a path-shaped one all
record — so the format is ours to choose.

>>> mkUri "example" ["All", "fib", "1000", "leaky"]
"example:All.fib.1000.leaky"

>>> mkUri "herbrand-sat-bench" ["All", "solve", "uf20-91"]
"herbrand-sat-bench:All.solve.uf20-91"

Names are left alone. A benchmark called @flat200-1.cnf@ already contains a dot,
so the result is not uniquely re-parseable — but mangling the name would make the
identifier lie about what it names, which is worse.
-}
mkUri :: String -> [String] -> String
mkUri component path = component <> ":" <> intercalate "." path

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
    component <- resolveComponent (configComponent cfg)
    announce sess component
    reportBuildEnvironment sess
    installSignalHandlers

    let instrumented = isInstrumented sess
        tree0 = instrumentTree component (testGroup "All" benchmarks)
        tree = if instrumented then tree0 else withDefaultTimeout tree0

    opts <- parseOptions benchIngredients tree
    -- Both mirror tasty-bench: benchmarks never run concurrently, and the
    -- console reporter should not editorialise about short runs.
    let opts' =
          setOption (NumThreads 1) $
            setOption (MinDurationToReport 1000000000000) opts

    evOn <- EL.eventlogEnabled
    sidecarRef <- newIORef []
    sidecarPath <- resolveSidecarPath (configSidecarPath cfg)
    ccsDir <- resolveEnvPath "CODSPEED_HS_CCS_DIR" (configCCSDir cfg)
    bracket_
      (writeIORef runnerRef (Just (RunnerState sess (configRootFrame cfg) evOn sidecarRef ccsDir)))
      -- tasty signals its result by throwing ExitCode, so the teardown of a
      -- bracket is the only place the sidecar can reliably be written.
      (flushSidecar sidecarPath sidecarRef >> writeIORef runnerRef Nothing)
      ( case tryIngredients benchIngredients opts' tree of
          Nothing -> exitFailure
          Just act -> act >>= \ok -> if ok then exitSuccess else exitFailure
      )

{- | Say on stderr whether this run is actually being measured.

Silence here is the failure mode worth guarding against. When the runner is not
detected the suite still runs, still prints results and still exits zero — it has
simply fallen back to @tasty-bench@'s own timing loop, and nothing is reported to
CodSpeed. That looks identical to success from CI, and the only symptom is an
empty run on the CodSpeed side, discovered much later.

Printing the component too, since a wrong one is the other quiet failure: the
benchmarks are reported, just under names that do not match the baseline, so
every one of them looks new.
-}
announce :: Session -> String -> IO ()
announce sess component
  | isInstrumented sess =
      hPutStrLn stderr $
        "[codspeed] measuring: component="
          <> component
          <> ", mode="
          <> mode
  | otherwise =
      hPutStrLn stderr $
        "[codspeed] NOT measuring: no runner detected, falling back to "
          <> "tasty-bench's own timing loop. Nothing will be reported to CodSpeed. "
          <> "(Run under `codspeed run` to measure.)"
  where
    -- Whether a runner is attached and what mode it announced are separate
    -- questions: under a bare valgrind the library activates while
    -- CODSPEED_RUNNER_MODE is unset, and printing "NotInstrumented" there while
    -- plainly measuring is worse than saying nothing.
    mode = case sessionMode sess of
      CS.NotInstrumented -> "unreported"
      m -> show m

-- | @configSidecarPath@, else @$CODSPEED_HS_SIDECAR@, else no local file.
resolveSidecarPath :: Maybe FilePath -> IO (Maybe FilePath)
resolveSidecarPath = resolveEnvPath "CODSPEED_HS_SIDECAR"

-- | An explicit setting, else a named environment variable, else nothing.
resolveEnvPath :: String -> Maybe FilePath -> IO (Maybe FilePath)
resolveEnvPath _ (Just p) = pure (Just p)
resolveEnvPath var Nothing = do
  fromEnv <- lookupEnv var
  pure $ case fromEnv of
    Just p | not (null p) -> Just p
    _ -> Nothing

{- | Emit whatever was measured.

Skipped entirely when nothing was recorded, so an ordinary un-instrumented run
does not litter the working directory with an empty file.
-}
flushSidecar :: Maybe FilePath -> IORef [Sidecar.Record] -> IO ()
flushSidecar path ref = do
  rs <- readIORef ref
  unless (null rs) (Sidecar.writeSidecar path rs)

{- | @tasty-bench@'s default 100-second per-benchmark cap, applied only when not
instrumented. An explicit timeout always wins.
-}
withDefaultTimeout :: TestTree -> TestTree
withDefaultTimeout = adjustOption $ \opt -> case opt of
  Timeout {} -> opt
  NoTimeout -> mkTimeout 100000000

{- | @configComponent@, else @$CODSPEED_HS_COMPONENT@, else the program name.

Cabal names a @benchmark foo@ stanza's binary @foo@, so the program name already
is the component name for any ordinary suite.
-}
resolveComponent :: Maybe String -> IO String
resolveComponent (Just c) = pure c
resolveComponent Nothing = do
  fromEnv <- lookupEnv "CODSPEED_HS_COMPONENT"
  case fromEnv of
    Just c | not (null c) -> pure c
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
