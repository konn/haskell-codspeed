{-# LANGUAGE CApiFFI #-}

{- | Raw bindings to CodSpeed's @instrument-hooks@ C library.

This module is a thin, unopinionated translation of @includes\/core.h@ plus the
shims in @cbits\/codspeed_shim.c@. Use "CodSpeed.Instrument" unless you need the
exact call sequence.

== Safety annotations are not incidental

Every function is annotated deliberately, because two of them sit at the edge of
the measured region and one of them is re-entrant.

[@unsafe@]: 'c_startBenchmark' and 'c_stopBenchmark'. A @safe@ call brackets the
  C call with @suspendThread@ \/ @resumeThread@, and @suspendThread@ runs
  @threadPaused@, which walks the entire Haskell stack. For 'c_startBenchmark'
  that walk would land on the @resumeThread@ side — i.e. /after/
  @CALLGRIND_START_INSTRUMENTATION@, which is the last statement of the C body —
  and so would be counted. For 'c_stopBenchmark' the @suspendThread@ side runs
  before the C body reaches @CALLGRIND_STOP_INSTRUMENTATION@, so it would also be
  counted. Both must be @unsafe@ or the measurement absorbs a stack walk whose
  cost scales with stack depth.

[@safe@]: everything doing IPC outside the window ('c_init', 'c_deinit',
  'c_setIntegration', 'c_setExecutedBenchmark', the environment calls). These can
  block on the runner's FIFO, and blocking a capability from an @unsafe@ call
  risks deadlock.

[@safe@]: 'c_rootFrame', which calls back into Haskell. Re-entrant calls
  /require/ @safe@.
-}
module CodSpeed.Instrument.Raw (
  -- * The opaque handle
  InstrumentHooks,

  -- * Lifecycle
  c_init,
  c_deinit,
  c_isInstrumented,
  c_setIntegration,

  -- * The measurement window
  c_startBenchmark,
  c_stopBenchmark,
  c_setExecutedBenchmark,

  -- * Markers
  markerSampleStart,
  markerSampleEnd,
  markerBenchmarkStart,
  markerBenchmarkEnd,
  c_addMarker,
  c_currentTimestamp,

  -- * Callgrind control
  c_callgrindStartInstrumentation,
  c_callgrindStopInstrumentation,
  c_callgrindToggleCollect,
  c_callgrindAddObjSkip,

  -- * Features
  featureDisableCallgrindMarkers,
  c_setFeature,

  -- * Environment reporting
  c_setEnvironment,
  c_setEnvironmentList,
  c_writeEnvironment,

  -- * Shims
  c_rootFrame,
  c_getpid,
  c_nonmovingGC,
) where

import Data.Word (Word64, Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Ptr (Ptr)
import Foreign.StablePtr (StablePtr)

{- | Opaque session handle owned by the C library.

A null pointer means initialisation failed; see 'c_init'.
-}
data InstrumentHooks

foreign import ccall safe "instrument_hooks_init"
  c_init :: IO (Ptr InstrumentHooks)

foreign import ccall safe "instrument_hooks_deinit"
  c_deinit :: Ptr InstrumentHooks -> IO ()

foreign import ccall unsafe "instrument_hooks_is_instrumented"
  c_isInstrumented :: Ptr InstrumentHooks -> IO CBool

-- | @0@ on success, as with every @uint8_t@-returning entry point here.
foreign import ccall safe "instrument_hooks_set_integration"
  c_setIntegration :: Ptr InstrumentHooks -> CString -> CString -> IO Word8

{- | Open the measurement window. Everything executed after this returns is
counted, so the caller must do as little as possible before the benchmark body.
-}
foreign import ccall unsafe "hs_codspeed_start_benchmark"
  c_startBenchmark :: Ptr InstrumentHooks -> IO Word8

-- | Close the measurement window.
foreign import ccall unsafe "hs_codspeed_stop_benchmark"
  c_stopBenchmark :: Ptr InstrumentHooks -> IO Word8

{- | Report which benchmark just ran.

Under the Valgrind instrument this is @CALLGRIND_DUMP_STATS_AT(uri)@, so the URI
is what ends up as @desc: Trigger: Client Request:@ on a @part:@ header in the
callgrind output — i.e. this call is what gives a benchmark its identity.
-}
foreign import ccall safe "instrument_hooks_set_executed_benchmark"
  c_setExecutedBenchmark :: Ptr InstrumentHooks -> CInt -> CString -> IO Word8

markerSampleStart, markerSampleEnd, markerBenchmarkStart, markerBenchmarkEnd :: Word8
markerSampleStart = 0
markerSampleEnd = 1
markerBenchmarkStart = 2
markerBenchmarkEnd = 3

{- | Delimit the benchmarked region in wall-clock time. Every @BENCHMARK_START@
must be matched by a @BENCHMARK_END@, in chronological order — the backend
rejects violations.

__A no-op under CPU simulation.__ The dispatch handles the walltime and analysis
instruments and falls through to success for everything else, writing nothing:
@cbits\/core.c@ around line 44264 tests @tag == 1@ then @tag == 2@, and the
valgrind instrument reaches @t7 = 0@ via @zig_block_5@. Every first-party
integration agrees — @pytest-codspeed@, @codspeed-node@, @-rust@, @-cpp@ and
@-go@ all confine markers to their walltime paths.

Emitted anyway, because they cost a call that writes nothing under simulation and
are the whole story under walltime, which this package does not support yet but
should. An earlier version of this comment claimed they were required under
simulation; that came from a probe whose result was never read.
-}
foreign import ccall unsafe "instrument_hooks_add_marker"
  c_addMarker :: Ptr InstrumentHooks -> CInt -> Word8 -> Word64 -> IO Word8

foreign import ccall unsafe "instrument_hooks_current_timestamp"
  c_currentTimestamp :: IO Word64

foreign import ccall unsafe "callgrind_start_instrumentation"
  c_callgrindStartInstrumentation :: IO ()

foreign import ccall unsafe "callgrind_stop_instrumentation"
  c_callgrindStopInstrumentation :: IO ()

{- | Toggle cost collection without re-instrumenting. Unlike stopping
instrumentation this does not flush the simulated cache, so it excludes a region
without charging it an artificial cold-cache penalty. A no-op outside Valgrind.
-}
foreign import ccall unsafe "callgrind_toggle_collect"
  c_callgrindToggleCollect :: IO ()

{- | Add an object file to Callgrind's runtime @--obj-skip@ list. A no-op outside
Valgrind.
-}
foreign import ccall safe "instrument_hooks_callgrind_add_obj_skip"
  c_callgrindAddObjSkip :: CString -> IO Word8

-- | @FEATURE_DISABLE_CALLGRIND_MARKERS@.
featureDisableCallgrindMarkers :: Word64
featureDisableCallgrindMarkers = 0

foreign import ccall unsafe "instrument_hooks_set_feature"
  c_setFeature :: Word64 -> CBool -> IO ()

foreign import ccall safe "instrument_hooks_set_environment"
  c_setEnvironment :: Ptr InstrumentHooks -> CString -> CString -> CString -> IO Word8

foreign import ccall safe "instrument_hooks_set_environment_list"
  c_setEnvironmentList ::
    Ptr InstrumentHooks -> CString -> CString -> Ptr CString -> CInt -> IO Word8

{- | Flush everything registered with 'c_setEnvironment' to
@$CODSPEED_PROFILE_FOLDER\/environment-\<pid\>.json@.
-}
foreign import ccall safe "instrument_hooks_write_environment"
  c_writeEnvironment :: Ptr InstrumentHooks -> CInt -> IO Word8

{- | Run a Haskell action beneath a C frame named @__codspeed_root_frame__hsBench@.

Must be @safe@: it re-enters the RTS through the @hs_codspeed_run_action@ foreign
export. See "CodSpeed.Instrument.RootFrame".
-}
foreign import ccall safe "__codspeed_root_frame__hsBench"
  c_rootFrame :: StablePtr (IO ()) -> IO ()

-- | @getpid@, avoiding a dependency on @unix@ and working on Windows.
foreign import ccall unsafe "hs_codspeed_getpid"
  c_getpid :: IO CInt

{- | Whether @--nonmoving-gc@ is in force.

Reads @RtsFlags.GcFlags.useNonmoving@ directly, because @GHC.RTS.Flags@ does not
expose it — the @nonmoving_gc@ fields it does have belong to @DebugFlags@ and
@TraceFlags@ and only govern tracing.
-}
foreign import ccall unsafe "hs_codspeed_nonmoving_gc"
  c_nonmovingGC :: IO Bool
