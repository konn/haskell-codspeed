{- | Guards the one place this package depends on another's private encoding.

@tasty-bench@ passes results between its runner and its reporters by @show@ing an
unexported @WithLoHi Estimate@ into @resultDescription@ and reading it back. A
replacement runner has to reproduce that string exactly, or @--csv@, @--baseline@,
@--svg@ and @bcompare@ all quietly stop working — quietly, because a reporter that
cannot parse a result just shows nothing useful rather than failing.

The golden strings below are what GHC's derived 'Show' produces for records, so
they encode field /names/ and /order/, not just values. Renaming a mirror field
breaks them, which is the point.
-}
module BenchEncodingSpec (
  test_encoding,
  test_roundTrip,
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Bench.CodSpeed.Internal
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

sample :: Estimate
sample = Estimate (Measurement 1234567 2048 512 4096) 0

test_encoding :: TestTree
test_encoding =
  testGroup
    "wire format"
    [ testCase "Measurement renders with tasty-bench's field names and order" $
        show (Measurement 1 2 3 4)
          @?= "Measurement {measTime = 1, measAllocs = 2, measCopied = 3, measMaxMem = 4}"
    , testCase "Estimate nests a Measurement under estMean" $
        show (Estimate (Measurement 1 2 3 4) 5)
          @?= "Estimate {estMean = Measurement {measTime = 1, measAllocs = 2,\
              \ measCopied = 3, measMaxMem = 4}, estStdev = 5}"
    , testCase "WithLoHi is positional, payload first" $
        encodeResult sample 0.9 1.1
          @?= "WithLoHi (Estimate {estMean = Measurement {measTime = 1234567,\
              \ measAllocs = 2048, measCopied = 512, measMaxMem = 4096},\
              \ estStdev = 0}) 0.9 1.1"
    , -- Derived Show parenthesises negative Doubles; tasty-bench's reader relies
      -- on that, and hand-rolling the string is exactly where it would be missed.
      testCase "negative bounds are parenthesised" $
        assertBool "expected parens around a negative bound" $
          "(-0.5)" `isInfixOf'` encodeResult sample (-0.5) 1.1
    ]

test_roundTrip :: TestTree
test_roundTrip =
  testGroup
    "round trip"
    [ testCase "read . show is identity for a plain estimate" $
        readWithLoHi (encodeResult sample 0.9 1.1) @?= WithLoHi sample 0.9 1.1
    , testCase "survives zero everywhere" $
        let z = Estimate (Measurement 0 0 0 0) 0
         in readWithLoHi (encodeResult z 1 1) @?= WithLoHi z 1 1
    , testCase "survives large values" $
        let big = Estimate (Measurement maxBound maxBound maxBound maxBound) maxBound
         in readWithLoHi (encodeResult big 0.5 2) @?= WithLoHi big 0.5 2
    , testCase "survives negative bounds" $
        readWithLoHi (encodeResult sample (-0.5) 1.5) @?= WithLoHi sample (-0.5) 1.5
    ]
  where
    readWithLoHi :: String -> WithLoHi Estimate
    readWithLoHi = read

isInfixOf' :: String -> String -> Bool
isInfixOf' needle haystack = any (needle `isPrefixOf'`) (tails' haystack)
  where
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (a : as) (b : bs) = a == b && isPrefixOf' as bs
    tails' [] = [[]]
    tails' s@(_ : rest) = s : tails' rest
