{- | Field-for-field copies of @tasty-bench@'s internal result types.

@tasty-bench@ encodes a benchmark's result by @show@ing a @WithLoHi Estimate@ into
'Test.Tasty.Providers.resultDescription', and its reporters @read@ it back out.
That is how @--csv@, @--baseline@, @--svg@ and @bcompare@ all work.

None of @Measurement@, @Estimate@ or @WithLoHi@ is exported, so a replacement
runner that wants those reporters to keep working has to reproduce the encoding.
These mirrors do that: identical constructors, identical field names, identical
field order, derived 'Show' and 'Read'. Derived 'Show' on a record emits field
names, so a renamed field silently breaks the format — hence the field-order
comment on each type and the round-trip test in @BenchIntegrationSpec@.

Pinned against @tasty-bench-0.5.1@ (@src\/Test\/Tasty\/Bench.hs@ lines 1057-1075).
The cabal bound is @>= 0.5 && < 0.6@; widening it means re-checking these.
-}
module Test.Tasty.Bench.CodSpeed.Internal (
  Measurement (..),
  Estimate (..),
  WithLoHi (..),
  encodeResult,
) where

import Data.Word (Word64)

{- | Mirror of @tasty-bench@'s @Measurement@.

Field order: @measTime@, @measAllocs@, @measCopied@, @measMaxMem@.
-}
data Measurement = Measurement
  { measTime :: !Word64
  -- ^ Picoseconds.
  , measAllocs :: !Word64
  -- ^ Bytes allocated.
  , measCopied :: !Word64
  -- ^ Bytes copied by the collector.
  , measMaxMem :: !Word64
  -- ^ Peak memory in use. A high-water mark, not a delta.
  }
  deriving (Show, Read, Eq)

{- | Mirror of @tasty-bench@'s @Estimate@.

Field order: @estMean@, @estStdev@.
-}
data Estimate = Estimate
  { estMean :: !Measurement
  , estStdev :: !Word64
  {- ^ Standard deviation in picoseconds.

  Always zero under instrumentation: the benchmark body runs exactly once, so
  there is no spread to report. @tasty-bench@ renders a zero stdev by omitting
  the @±@ column entirely, which is the honest presentation.
  -}
  }
  deriving (Show, Read, Eq)

{- | Mirror of @tasty-bench@'s @WithLoHi@.

Positional, not a record: payload, lower bound, upper bound. The bounds come from
@FailIfFaster@ and @FailIfSlower@ and are what the console reporter uses to decide
whether a comparison counts as a regression.
-}
data WithLoHi a = WithLoHi !a !Double !Double
  deriving (Show, Read, Eq)

{- | Render a result the way @tasty-bench@'s reporters expect to read it.

>>> encodeResult (Estimate (Measurement 1000 2048 0 0) 0) 0.9 1.1
"WithLoHi (Estimate {estMean = Measurement {measTime = 1000, measAllocs = 2048, measCopied = 0, measMaxMem = 0}, estStdev = 0}) 0.9 1.1"
-}
encodeResult :: Estimate -> Double -> Double -> String
encodeResult est lo hi = show (WithLoHi est lo hi)
