{- | Running benchmark code beneath a C frame named @__codspeed_root_frame__*@.

CodSpeed asks that benchmarked code execute inside a non-inlined function whose
/symbol/ begins with @__codspeed_root_frame__@, so its flamegraphs have a clean
root. GHC mangles every Haskell binding to @\<pkg\>_\<Module\>_\<occ\>_info@, so no
Haskell function can ever carry that name. The only way to satisfy it is to bounce
out to C and back.

== The cost, which is paid anyway

This was long treated as a flamegraph nicety, on the reading that CodSpeed's
Valgrind fork mentions the name only in comments about re-parenting and that the
runner never inspects it. That reading is wrong. Replacing the frame call with a
direct call in upstream's own @example\/main.c@ — one token, nothing else
touched — yields a profile with correct URIs and 288M @Ir@ that the backend
accepts no benchmark from, in the same run as a control that records fine. The
requirement is real and the documentation meant it literally.

So the cost below is not optional, but it is worth knowing about.
'withRootFrame' re-enters the RTS through a foreign export, which means
@rts_lock@ and a fresh bound Haskell thread for every call:

* __The action runs on a different Haskell thread than the caller.__ An
  asynchronous exception aimed at the calling thread — @System.Timeout.timeout@,
  and therefore tasty's @--timeout@ — will not interrupt it.
* Thread identity changes, so anything keyed on 'Control.Concurrent.myThreadId'
  sees a different value inside.
* There is a fixed setup cost per call, and it is counted — see below.

== Where the window opens

The window opens /before/ this is entered, so @rts_lock@ and bound-thread setup
are inside the counted region.

That is the opposite of what this module originally did, and the reversal is
forced. Callgrind records calls made after @CALLGRIND_START_INSTRUMENTATION@ and
does not reconstruct the frames already on the stack, so a root frame entered
first is a frame it never sees: with the feature enabled, the profile contained
no @__codspeed_root_frame__@ at all. A frame that is invisible to the profile
cannot satisfy a requirement about the profile.

The cost that buys is fixed and deterministic — the same in every measurement,
in the same class as any other harness overhead inside the window — and
upstream's example pays it identically.
-}
module CodSpeed.Instrument.RootFrame (
  withRootFrame,
) where

import CodSpeed.Instrument.Raw (c_rootFrame)
import Control.Exception (SomeException, bracket, throwIO, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)

{- | The callback C invokes. Exported under a fixed name so
@cbits\/codspeed_shim.c@ can declare it.

Nothing may escape this as an exception: an uncaught exception crossing an RTS
in-call boundary terminates the process outright rather than propagating. So the
result — success or failure — is stashed in an 'Data.IORef.IORef' and re-raised by
'withRootFrame' on the far side.
-}
foreign export ccall "hs_codspeed_run_action"
  runActionFromC :: StablePtr (IO ()) -> IO ()

runActionFromC :: StablePtr (IO ()) -> IO ()
runActionFromC sp = do
  act <- deRefStablePtr sp
  act

{- | Run an action beneath @__codspeed_root_frame__hsBench@.

Semantically transparent apart from the thread-identity caveats above: the
result is returned, and an exception thrown by the action is re-raised here.
-}
withRootFrame :: IO a -> IO a
withRootFrame act = do
  slot <- newIORef Nothing
  let body :: IO ()
      body = do
        r <- try act
        writeIORef slot (Just r)
  bracket (newStablePtr body) freeStablePtr c_rootFrame
  outcome <- readIORef slot
  case outcome of
    Just (Right a) -> pure a
    Just (Left err) -> throwIO (err :: SomeException)
    -- Only reachable if the C shim returned without invoking the callback,
    -- which would mean the shim and this module have drifted apart.
    Nothing ->
      throwIO . userError $
        "CodSpeed.Instrument.RootFrame: __codspeed_root_frame__hsBench "
          <> "returned without running the action"
