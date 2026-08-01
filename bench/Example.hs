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
import Test.Tasty.Bench.CodSpeed

main :: IO ()
main =
  defaultMain
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
        [ bench (show n) (nf fib n)
        | n <- [15, 20 :: Int]
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

fib :: Int -> Integer
fib n = go n 0 1
  where
    go :: Int -> Integer -> Integer -> Integer
    go 0 a _ = a
    go k a b = go (k - 1) b (a + b)
