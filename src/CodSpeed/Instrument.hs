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
  pvpToSemver,
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
import Control.Exception (bracket, throwIO)
import Control.Monad (unless, void, when)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Version (Version (..))
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
  -- ^ Free-form. Probed: @haskell-codspeed@ is accepted.
  , integrationVersion :: String
  {- ^ __Semver, @x.y.z@ — three components.__

  Not the Cabal version. Haskell uses PVP, so the natural thing to reach for
  here is something like @0.1.0.0@, and a fourth component makes CodSpeed
  discard the entire run.

  Silently, and at a distance. The benchmarks run, the profile is written with
  every URI and every cost in it, all return codes are zero, the runner logs a
  successful upload and CI goes green; the run then reports "this run could not
  be processed" with no indication of why. Changing @0.1.0.0@ to @0.1.0@, one
  token, took this package's example suite from nothing recorded to all eight
  benchmarks.

  Use 'pvpToSemver' rather than picking a string by hand.
  -}
  }
  deriving (Show, Eq)

{- | Carry a PVP version into semver without dropping a component.

@A.B.C.D@ becomes @A.B.C+pvp.D@, and anything PVP adds beyond the fourth
component comes along:

>>> import Data.Version (makeVersion)
>>> pvpToSemver (makeVersion [0, 1, 0, 0])
"0.1.0+pvp.0"

>>> pvpToSemver (makeVersion [1, 2, 3, 4, 5])
"1.2.3+pvp.4.5"

== Why build metadata and not a pre-release

@+@ rather than @-@, deliberately. Semver §9 says a pre-release "indicates that
the version is unstable and might not satisfy the intended compatibility
requirements", and orders it /below/ the plain version — so @0.1.0-pvp.0@ would
mark every released Haskell package as unstable and sort it beneath a @0.1.0@ no
Haskell package would ever publish.

§10 says build metadata is ignored for precedence, which is what PVP's fourth
component already means: @A.B@ covers breaking changes, @C@ additions, and @D@ is
reserved for changes that do not affect the API at all. A component that carries
no compatibility information belongs in the part of the version that carries no
compatibility information.

The cost, which is real: @0.1.0.0@ and @0.1.0.1@ map to versions of /equal/
precedence, so a consumer comparing them properly cannot tell them apart. They
remain distinct strings, and this one identifies an integration in a UI rather
than resolving dependencies, so that seemed the better trade than claiming
instability.

@0.1.0+pvp.0@ is accepted: this package's own suite records all eight benchmarks
with it. Which incidentally settles what the backend does with this field — it
parses semver properly rather than matching @x.y.z@, since build metadata
survives. A @-pvp.D@ pre-release would therefore very likely be accepted too; the
choice between them is the semantic one above, not a compatibility one.

== Edge cases

A version already the shape of a semver core is left alone, rather than growing
an empty @+pvp.@:

>>> pvpToSemver (makeVersion [2, 7, 1])
"2.7.1"

Short versions are padded, since semver requires all three components:

>>> pvpToSemver (makeVersion [1, 4])
"1.4.0"

>>> pvpToSemver (makeVersion [3])
"3.0.0"

The empty version becomes @0.0.0@ rather than an error. A meaningless version
string is much cheaper than a malformed one, given what the backend does with a
malformed one.

>>> pvpToSemver (makeVersion [])
"0.0.0"

Build-metadata identifiers must be alphanumerics and hyphens. These come from
'versionBranch', which is @[Int]@, so 'show' cannot produce anything else.
-}
pvpToSemver :: Version -> String
pvpToSemver v = core <> metadata
  where
    (top, rest) = splitAt 3 (versionBranch v)
    core = intercalate "." (map show (take 3 (top <> repeat 0)))
    metadata
      | null rest = ""
      | otherwise = "+pvp." <> intercalate "." (map show rest)

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
    -- Before the benchmarks, per the documented lifecycle. See
    -- 'reportIntegration' for why it briefly was not.
    reportIntegration sess integration
    act sess
  where
    acquire = c_init
    release hooks = unless (hooks == nullPtr) (c_deinit hooks)

{- | Tell CodSpeed which integration produced these results.

Called once, before any benchmark, as @CUSTOM_HARNESS.md@ specifies.

This was briefly moved to /after/ the benchmarks, on a local measurement that
appeared to show a dump issued before @CALLGRIND_START_INSTRUMENTATION@ leaving
Callgrind unable to instrument anything afterwards — @totals: 0@ with the call
first, non-zero with it last. That measurement does not survive contact with
CodSpeed's own example: built and run through the identical CI pipeline,
@instrument-hooks@'s @example\/main.c@ calls this first, its metadata lands in
@part: 1@ with @totals: 0@, and @part: 2@ still accumulates 288,719,342 @Ir@ —
on the same @callgrind-3.26.0.codspeed6@, on the same runner image. So the dump
does not poison later instrumentation, and the probe that said otherwise was
measuring something else.

Whether the backend cares about the ordering is untested — a later probe claiming
it was fatal turned out never to have been read. Every integration CodSpeed ships
calls this at init, which is reason enough to do the same.

__What the backend does care about is the version string.__ It must be semver:
@0.1.0.0@ — a perfectly ordinary Haskell package version — makes the whole run
be discarded, silently. See 'Integration'.
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

  On by default, but /not/ because a run without it is rejected — it is not.
  CodSpeed's own @exec-harness@ has no root frame anywhere and records fine.

  It is on because it is what shapes the flamegraph, which is the reason this
  package exists. Every language integration arranges one: @pytest-codspeed@ a
  nested Python closure, @codspeed-node@ a named JS function, @codspeed-rust@,
  @-cpp@ and @-go@ real symbols.

  (An earlier version of this comment said omitting it was fatal, on a probe
  whose profile the backend never read. See @README.md@ on @GH_MATRIX@.)

  Costs an RTS in-call per benchmark, which moves the body onto a fresh bound
  thread and so puts it out of reach of @System.Timeout.timeout@. See
  "CodSpeed.Instrument.RootFrame".

  The in-call happens /inside/ the measurement window, so @rts_lock@ and
  bound-thread setup are counted. That is deliberate and unavoidable: Callgrind
  only records calls made after @CALLGRIND_START_INSTRUMENTATION@, so a frame
  entered before the window opens is invisible to it. Entering it beforehand —
  which is what this did until the profile was actually read — produced a run
  with the flag on and no @__codspeed_root_frame__@ anywhere in the output.

  The cost is a fixed, deterministic offset, of the same kind as any other
  harness overhead inside the window. Upstream's own example pays it the same
  way.
  -}
  , optPerformGC :: !Bool
  {- ^ Run a major GC immediately before opening the window. On by default.

  Without it, whatever garbage the previous benchmark left behind is collected
  inside /this/ benchmark's window, which both inflates the count and couples
  neighbouring benchmarks to each other.
  -}
  }
  deriving (Show, Eq)

-- | Both on. The root frame is not a nicety — see 'optRootFrame'.
defaultOptions :: Options
defaultOptions = Options {optRootFrame = True, optPerformGC = True}

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

== The shape of the window

Four things about the sequence below are load-bearing, each established by
deleting it from upstream's @example\/main.c@ and watching the backend accept no
benchmark from the result while an otherwise identical control recorded:

* the benchmark runs beneath a @__codspeed_root_frame__@ frame,
* the region is delimited by a @BENCHMARK_START@\/@BENCHMARK_END@ marker pair,
* both happen /after/ @start_benchmark@, so Callgrind actually sees them,
* and the integration metadata was reported before any of this (see
  'reportIntegration').

Nothing reports an error when one is missing. The benchmarks run, the profile is
written with all the right URIs and non-zero costs in it, every return code is
zero, and the run is rejected server-side.
-}
benchmarkWith :: Options -> Session -> String -> IO () -> IO ()
benchmarkWith opts sess uri act
  | not (sessionInstrumented sess) = act
  | otherwise = do
      when (optPerformGC opts) performGC
      -- Everything from here to c_stopBenchmark is counted. The rc check is a
      -- couple of instructions and buys us not reporting a bogus measurement.
      --
      -- `wrap` is inside the window rather than around it, because Callgrind
      -- only sees calls made after instrumentation starts. See 'optRootFrame'.
      rc <- c_startBenchmark hooks
      when (rc == 0) $ do
        t0 <- c_currentTimestamp
        wrap act
        t1 <- c_currentTimestamp
        marker markerBenchmarkStart t0
        marker markerBenchmarkEnd t1
      stopRC <- c_stopBenchmark hooks
      checkRC "start_benchmark" rc
      checkRC "stop_benchmark" stopRC
      withCString uri $ \curi ->
        checkRC "set_executed_benchmark"
          =<< c_setExecutedBenchmark hooks (sessionPid sess) curi
  where
    hooks = sessionHooks sess
    wrap = if optRootFrame opts then withRootFrame else id

    -- Return code deliberately ignored, as upstream's example ignores it. A
    -- marker that could not be delivered -- no runner listening on the walltime
    -- FIFO, say -- is not a reason to fail a measurement that otherwise
    -- succeeded, and this runs inside the counted window where throwing would
    -- leave the instrument started.
    marker ty t = void (c_addMarker hooks (sessionPid sess) ty t)

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
