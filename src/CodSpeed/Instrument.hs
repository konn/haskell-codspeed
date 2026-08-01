{- | A safe interface to CodSpeed's measurement hooks.

The shape of a run is:

@
'withSession' myIntegration $ \\sess -> do
  'reportEnvironment' sess "haskell" [("ghc", "9.12.4")]
  forM_ benchmarks $ \\(uri, act) ->
    'benchmark' sess uri act
@

Everything degrades to a no-op when the process is not running under the CodSpeed
runner, so a benchmark suite built on this can be run directly without ceremony.
Check 'isInstrumented' if you want to take a different path in that case — a
framework should fall back to its own timing loop rather than measuring once.
-}
module CodSpeed.Instrument (
  -- * Sessions
  Session,
  sessionMode,
  sessionPid,
  isInstrumented,
  Integration (..),
  withSession,

  -- * Modes
  Mode (..),
  detectMode,
  parseMode,

  -- * Measuring
  benchmark,
  benchmarkWith,
  Options (..),
  defaultOptions,

  -- * Reporting metadata
  reportEnvironment,
  reportEnvironmentList,
  writeEnvironment,

  -- * Provenance
  instrumentHooksCommit,
) where

import CodSpeed.Instrument.Raw
import CodSpeed.Instrument.RootFrame (withRootFrame)
import CodSpeed.InstrumentHooks.Vendor (instrumentHooksCommit)
import Control.Exception (bracket, finally, throwIO)
import Control.Monad (unless, when)
import Data.Char (toLower)
import Data.Word (Word8)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CBool (..), CInt)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, nullPtr)
import System.Environment (lookupEnv)
import System.Mem (performGC)

{- | Which instrument the runner is driving.

The runner communicates this through @CODSPEED_RUNNER_MODE@.
-}
data Mode
  = -- | Not running under CodSpeed. Every operation here is a no-op.
    NotInstrumented
  | {- | Deterministic CPU simulation under CodSpeed's Valgrind fork.

    The runner still spells this @instrumentation@ on the wire; both spellings
    are accepted.
    -}
    Simulation
  | -- | Real elapsed time, sampled by @samply@ or @perf@.
    Walltime
  | {- | Heap allocation tracing via eBPF.

    Of little use for GHC: the RTS acquires memory in large blocks and
    sub-allocates its own heap, so Haskell-level allocation never reaches
    @malloc@ and is invisible to this instrument. Use @GHC.Stats@ instead.
    -}
    Memory
  | -- | A mode this library does not recognise. Treated like 'Simulation'.
    UnknownMode String
  deriving (Show, Eq)

-- | Who is reporting. Surfaces in the CodSpeed UI as the integration's identity.
data Integration = Integration
  { integrationName :: String
  , integrationVersion :: String
  }
  deriving (Show, Eq)

-- | An open connection to the runner. Create with 'withSession'.
data Session = Session
  { sessionHooks :: !(Ptr InstrumentHooks)
  , sessionMode :: !Mode
  -- ^ The instrument in force for this run.
  , sessionPid :: !CInt
  -- ^ This process's pid, as reported to the runner.
  , sessionInstrumented :: !Bool
  }

-- | Whether the runner is actually listening.
isInstrumented :: Session -> Bool
isInstrumented = sessionInstrumented

{- | Read @CODSPEED_RUNNER_MODE@.

Returns 'NotInstrumented' when unset. Note this reflects only what the environment
says; 'isInstrumented' is the authoritative answer, since it asks the C library.
-}
detectMode :: IO Mode
detectMode = parseMode <$> lookupEnv "CODSPEED_RUNNER_MODE"

{- | The pure half of 'detectMode', given the raw value of
@CODSPEED_RUNNER_MODE@.

Matching is case-insensitive, and an unset or empty value both mean
'NotInstrumented'.

>>> parseMode Nothing
NotInstrumented
>>> parseMode (Just "walltime")
Walltime
>>> parseMode (Just "Simulation")
Simulation

The runner writes @instrumentation@ where the CLI and docs say @simulation@, so
both spellings map to 'Simulation':

>>> parseMode (Just "instrumentation") == parseMode (Just "simulation")
True
-}
parseMode :: Maybe String -> Mode
parseMode raw = case fmap (map toLower) raw of
  Nothing -> NotInstrumented
  Just "" -> NotInstrumented
  Just "instrumentation" -> Simulation
  Just "simulation" -> Simulation
  Just "walltime" -> Walltime
  Just "memory" -> Memory
  Just other -> UnknownMode other

{- | Open a session, run the body, and close it.

Safe to use unconditionally: outside CodSpeed this still succeeds, and the
resulting 'Session' reports 'isInstrumented' as 'False'.
-}
withSession :: Integration -> (Session -> IO a) -> IO a
withSession integration act =
  bracket acquire release $ \hooks -> do
    instrumented <-
      if hooks == nullPtr
        then pure False
        else toBool <$> c_isInstrumented hooks
    mode <- detectMode
    pid <- c_getpid
    let sess =
          Session
            { sessionHooks = hooks
            , sessionMode = mode
            , sessionPid = pid
            , sessionInstrumented = instrumented
            }
    -- Registered last, not first. See 'reportIntegration'. `finally` because the
    -- body normally leaves by throwing ExitCode.
    act sess `finally` reportIntegration sess integration
  where
    acquire = c_init
    release hooks = unless (hooks == nullPtr) (c_deinit hooks)

{- | Tell CodSpeed which integration produced these results.

__Must run after the benchmarks, never before.__ Under the CPU-simulation
instrument this is a @CALLGRIND_DUMP_STATS_AT@, and the runner starts Valgrind
with @--instr-atstart=no@. Issuing a dump while instrumentation is still off
leaves Callgrind in a state where @CALLGRIND_START_INSTRUMENTATION@ never takes
effect again, so every subsequent measurement records zero.

Nothing reports an error when that happens. The benchmarks run, the profile is
written with all the right benchmark URIs in it, every return code is 0, the
runner uploads happily and the job goes green — and the backend rejects the run
with "this run could not be processed", because every cost in it is zero.

Measured, on CodSpeed's own Valgrind fork, two identical benchmarks differing
only in when this call happens:

@
set_integration first:  totals: 0
set_integration last:   totals: 6000008
@

Upstream's @example\/main.c@ calls it first, which is where the original ordering
came from.
-}
reportIntegration :: Session -> Integration -> IO ()
reportIntegration sess integration
  | not (sessionInstrumented sess) = pure ()
  | otherwise =
      withCString (integrationName integration) $ \cname ->
        withCString (integrationVersion integration) $ \cver ->
          checkRC "set_integration" =<< c_setIntegration (sessionHooks sess) cname cver

-- | Knobs for a single measurement. Start from 'defaultOptions'.
data Options = Options
  { optRootFrame :: !Bool
  {- ^ Wrap the body in a @__codspeed_root_frame__@ C frame.

  Off by default. It buys a tidy flamegraph root at the cost of an RTS in-call
  per benchmark, which moves the body onto a fresh bound thread and so puts it
  out of reach of @System.Timeout.timeout@. See "CodSpeed.Instrument.RootFrame".
  -}
  , optPerformGC :: !Bool
  {- ^ Run a major GC immediately before opening the window. On by default.

  Without it, whatever garbage the previous benchmark left behind is collected
  inside /this/ benchmark's window, which both inflates the count and couples
  neighbouring benchmarks to each other.
  -}
  }
  deriving (Show, Eq)

-- | Root frame off, GC on.
defaultOptions :: Options
defaultOptions = Options {optRootFrame = False, optPerformGC = True}

-- | 'benchmarkWith' at 'defaultOptions'.
benchmark :: Session -> String -> IO () -> IO ()
benchmark = benchmarkWith defaultOptions

{- | Measure one benchmark and report it under @uri@.

@uri@ should follow CodSpeed's convention of
@{git-relative-path}::{name}::{components}@.

When the session is not instrumented the action is run exactly once and nothing
is reported, so callers get the side effects without the bookkeeping.

The action is run precisely once. For a lazy language that places the burden on
the caller: whatever the action does must genuinely force the work under test, and
any input it depends on should already be evaluated before this is called.
-}
benchmarkWith :: Options -> Session -> String -> IO () -> IO ()
benchmarkWith opts sess uri act
  | not (sessionInstrumented sess) = act
  | otherwise = wrap $ do
      when (optPerformGC opts) performGC
      -- Everything from here to c_stopBenchmark is counted. The rc check is a
      -- couple of instructions and buys us not reporting a bogus measurement.
      rc <- c_startBenchmark hooks
      when (rc == 0) act
      stopRC <- c_stopBenchmark hooks
      checkRC "start_benchmark" rc
      checkRC "stop_benchmark" stopRC
      withCString uri $ \curi ->
        checkRC "set_executed_benchmark"
          =<< c_setExecutedBenchmark hooks (sessionPid sess) curi
  where
    hooks = sessionHooks sess
    wrap = if optRootFrame opts then withRootFrame else id

{- | Register key/value metadata under a named section.

Flushed to @$CODSPEED_PROFILE_FOLDER\/environment-\<pid\>.json@ by
'writeEnvironment'.

Worth recording the things that silently invalidate a baseline: the GHC version,
the RTS flags in force, @-A@ in particular, and the optimisation level. A change
to any of them shifts instruction counts exactly as a code change would.
-}
reportEnvironment :: Session -> String -> [(String, String)] -> IO ()
reportEnvironment sess section entries
  | not (sessionInstrumented sess) = pure ()
  | otherwise =
      withCString section $ \csection ->
        mapM_ (one csection) entries
  where
    one csection (k, v) =
      withCString k $ \ck ->
        withCString v $ \cv ->
          checkRC "set_environment" =<< c_setEnvironment (sessionHooks sess) csection ck cv

-- | As 'reportEnvironment', for a key with several values.
reportEnvironmentList :: Session -> String -> String -> [String] -> IO ()
reportEnvironmentList sess section key values
  | not (sessionInstrumented sess) = pure ()
  | otherwise =
      withCString section $ \csection ->
        withCString key $ \ckey ->
          withCStrings values $ \arr ->
            withArray arr $ \parr ->
              checkRC "set_environment_list"
                =<< c_setEnvironmentList
                  (sessionHooks sess)
                  csection
                  ckey
                  parr
                  (fromIntegral (length values))

-- | Flush everything registered by 'reportEnvironment' to the profile folder.
writeEnvironment :: Session -> IO ()
writeEnvironment sess
  | not (sessionInstrumented sess) = pure ()
  | otherwise =
      checkRC "write_environment"
        =<< c_writeEnvironment (sessionHooks sess) (sessionPid sess)

-- | Bracket a list of Haskell strings as a contiguous run of live 'CString's.
withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings = go []
  where
    go acc [] k = k (reverse acc)
    go acc (s : ss) k = withCString s $ \cs -> go (cs : acc) ss k

{- | Every @instrument_hooks_*@ entry point returns @0@ for success; anything else
means the conversation with the runner broke down, which is not something a
benchmark run should quietly continue past.
-}
checkRC :: String -> Word8 -> IO ()
checkRC what rc =
  unless (rc == 0) . throwIO . userError $
    "CodSpeed: instrument_hooks_" <> what <> " failed (rc=" <> show rc <> ")"

toBool :: CBool -> Bool
toBool (CBool w) = w /= 0
