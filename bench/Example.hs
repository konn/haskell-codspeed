{- | A small benchmark suite exercising the drop-in runner.

Run it directly and it behaves like any @tasty-bench@ suite. Run it under
@codspeed run@ and each leaf becomes its own CodSpeed benchmark.

@
cabal bench example
codspeed run --mode simulation --skip-upload -- cabal bench example
@

The only difference from a plain @tasty-bench@ suite is the import.
-}
module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Test.Tasty.Bench.CodSpeed

main :: IO ()
main = do
  cfg <- probeConfig
  defaultMainWith cfg suite

{- | Diagnostic only: override the reported integration from the environment.

CodSpeed renders a flamegraph for @codspeed-cpp@ and @pytest-codspeed@ runs and
not for this one, and under CPU simulation the integration @Metadata:@ line is the
only identity a profile carries. CodSpeed's own documentation lists profiling
availability per named integration with minimum versions, which can only be a
server-side lookup on that string — so the name is the remaining candidate for
what gates it.

Here rather than in the library because it exists to answer one question. Nothing
should ever ship reporting a name that is not its own.
-}
probeConfig :: IO Config
probeConfig = do
  name <- lookupEnv "CODSPEED_HS_PROBE_INTEGRATION"
  ver <- lookupEnv "CODSPEED_HS_PROBE_VERSION"
  let base = configIntegration defaultConfig
  pure
    defaultConfig
      { configIntegration =
          base
            { integrationName = fromMaybe (integrationName base) name
            , integrationVersion = fromMaybe (integrationVersion base) ver
            }
      }

suite :: [Benchmark]
suite =
  [ bgroup
      "sort"
      [ -- env forces its result to normal form before the benchmark runs, so
      -- building the input is not charged to the measurement. That matters
      -- more here than under stock tasty-bench: with a single instrumented
      -- iteration there is no warm-up run to absorb it.
      env (evaluate (force (descending n))) $ \xs ->
        bench (show n) (nf sort xs)
      | n <- [1000, 10000 :: Int]
      ]
  , bgroup
      "fib"
      -- The same recursion twice, differing only in whether the accumulators
      -- are forced. The lazy one builds a chain of n unevaluated additions on
      -- the heap; the strict one runs in constant space. Allocation separates
      -- them completely, and allocation is the metric that is exactly
      -- reproducible -- so this is the shape of regression the sidecar gate
      -- exists to catch.
      [ bgroup
          (show n)
          [ bench "leaky" (nf fibLeaky n)
          , bench "strict" (nf fibStrict n)
          ]
      | n <- [1000, 10000 :: Int]
      ]
  , bgroup
      "allocation"
      [ -- Deliberately contrasting shapes: `nf sum . enumFromTo` fuses at -O2
        -- into an unboxed loop that allocates nothing, while the reverse
        -- materialises the whole list. Under CodSpeed the two look very
        -- different, which is the point.
        bench "fused (allocates nothing)" (nf sumTo 100000)
      , bench "materialised" (nf sumReversed 100000)
      ]
  ]

-- | Fuses at @-O2@ into an unboxed accumulator loop: no heap allocation at all.
sumTo :: Int -> Int
sumTo k = sum [1 .. k]

{- | 'reverse' is not a good producer, so the list is genuinely materialised —
roughly @24 * k@ bytes of cons cells.
-}
sumReversed :: Int -> Int
sumReversed k = sum (reverse [1 .. k])

descending :: Int -> [Int]
descending n = [n, n - 1 .. 1]

{- | Fibonacci by accumulator, leaving the accumulator unforced.

@a + b@ is built as a thunk rather than evaluated, so after @n@ steps the heap
holds a chain of @n@ pending additions, collapsed only when the result is finally
demanded. The canonical Haskell space leak.

'Int' rather than 'Integer' deliberately: the arithmetic is then constant-cost, so
the difference against 'fibStrict' is the thunks and nothing else.
-}
fibLeaky :: Int -> Int
fibLeaky n = go n 0 1
  where
    go :: Int -> Int -> Int -> Int
    go 0 a _ = a
    go k a b = go (k - 1) b (a + b)

-- | The same recursion, forcing each accumulator as it goes.
fibStrict :: Int -> Int
fibStrict n = go n 0 1
  where
    go :: Int -> Int -> Int -> Int
    go 0 a _ = a
    go k !a !b = go (k - 1) b (a + b)
