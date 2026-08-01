{- | Marking benchmark boundaries in the GHC eventlog.

CodSpeed answers /which benchmark/ regressed. The eventlog answers /what the
runtime was doing while it did/ — GC pauses, heap growth, thread activity — and
@-hT@ heap profiling works without a profiling build. Emitting a marker around
each benchmark is what lets those two be lined up: @eventlog2html@, ThreadScope
and @ghc-events-analyze@ can then all slice a run per benchmark instead of
presenting one undifferentiated timeline.

== Gated, because the markers are not free

'Debug.Trace.traceEventIO' allocates on the GHC heap whether or not an eventlog is
being written — measured at roughly 1-3.5 kB per call on GHC 9.12, against a
176-byte baseline for @pure ()@. A benchmark run exactly once has no iteration
count to divide that away, so unconditional markers would be a real and
deterministic distortion of the allocation metric.

'eventlogEnabled' therefore checks the RTS trace flags, and 'withRegion' does
nothing at all unless user events are actually being recorded. Callers should also
emit outside the measurement bracket; 'withRegion' is designed to wrap it, not to
sit inside it.

== Label format

Labels follow the convention @ghc-events-analyze@ expects: @\"START \"@ and
@\"STOP \"@ prefixes, with the trailing space, on a @UserMessage@.

The literal @codspeed@ token after the prefix is not decoration. That tool's
parser tries a signed decimal first, so a label beginning with a digit has its
leading digits consumed as a subscript — a benchmark named @1000@ would be
silently mis-attributed. Prefixing every label sidesteps it.

@
START codspeed bench\/Bench.hs::All::sort::1000
STOP codspeed bench\/Bench.hs::All::sort::1000
@
-}
module CodSpeed.RTS.Eventlog (
  eventlogEnabled,
  withRegion,
  startLabel,
  stopLabel,
) where

import Control.Exception (onException)
import Debug.Trace (flushEventLog, traceEventIO)
import GHC.RTS.Flags (DoTrace (..), TraceFlags (..), getTraceFlags)

{- | Whether user events emitted now would actually be recorded.

True only when the program was run with an eventlog-writing @-l@ that includes
user events. Everything in this module is a no-op otherwise.
-}
eventlogEnabled :: IO Bool
eventlogEnabled = do
  flags <- getTraceFlags
  -- DoTrace has no Eq instance, hence the match rather than (==).
  pure $ case tracing flags of
    TraceEventLog -> user flags
    TraceStderr -> False
    TraceNone -> False

{- | The label opening a benchmark's region.

>>> startLabel "bench/Bench.hs::All::sort::1000"
"START codspeed bench/Bench.hs::All::sort::1000"
-}
startLabel :: String -> String
startLabel uri = "START codspeed " <> uri

{- | The label closing it.

>>> stopLabel "bench/Bench.hs::All::sort::1000"
"STOP codspeed bench/Bench.hs::All::sort::1000"
-}
stopLabel :: String -> String
stopLabel uri = "STOP codspeed " <> uri

{- | Bracket an action with eventlog markers, if the eventlog is on.

Wrap this /around/ the measurement rather than inside it, so the markers' own
allocation is not charged to the benchmark.

The log is flushed after each region. Without that, a run killed by a CI timeout —
easy to hit at a 200x simulation slowdown — loses its entire eventlog, since
@-V0@ (which the preflight checks recommend) also disables the periodic flush.

A @STOP@ is emitted even when the action fails: @ghc-events-analyze@ pairs
markers by name, and an unclosed @START@ corrupts the rest of the parse rather
than just losing one region.
-}
withRegion :: Bool -> String -> IO a -> IO a
withRegion False _ act = act
withRegion True uri act = do
  traceEventIO (startLabel uri)
  r <- act `onException` closeRegion
  closeRegion
  pure r
  where
    closeRegion = do
      traceEventIO (stopLabel uri)
      flushEventLog
