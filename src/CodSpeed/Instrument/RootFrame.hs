{- | Running benchmark code beneath a C frame named @__codspeed_root_frame__*@.

CodSpeed asks that benchmarked code execute inside a non-inlined function whose
/symbol/ begins with @__codspeed_root_frame__@, so its flamegraphs have a clean
root. GHC mangles every Haskell binding to @\<pkg\>_\<Module\>_\<occ\>_info@, so no
Haskell function can ever carry that name. The only way to satisfy it is to bounce
out to C and back.

== The cost, and why this is off by default

'withRootFrame' re-enters the RTS through a foreign export, which means
@rts_lock@ and a fresh bound Haskell thread for every call. Three consequences,
all of which argue for keeping this opt-in:

* __The action runs on a different Haskell thread than the caller.__ An
  asynchronous exception aimed at the calling thread — @System.Timeout.timeout@,
  and therefore tasty's @--timeout@ — will not interrupt it.
* Thread identity changes, so anything keyed on 'Control.Concurrent.myThreadId'
  sees a different value inside.
* There is a fixed setup cost per call. It lands /outside/ the measurement window
  (see below), so it does not corrupt the count, but it is not free in wall time.

Evidence from CodSpeed's Valgrind fork suggests the root frame shapes the call
/graph/ rather than the /counter/ — it reconstructs the native stack at
@CALLGRIND_START_INSTRUMENTATION@ and the runner never inspects the name. So
omitting it should cost flamegraph tidiness and nothing else. That is the reason
for the default; spike S0 is what confirms it.

== Where the window opens

The measurement window is opened and closed by the caller's action, /inside/ the
callback, not around 'withRootFrame'. That ordering is deliberate: it keeps
@rts_lock@ and bound-thread setup outside the counted region, while still leaving
the C frame on the native stack for the whole window.
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
