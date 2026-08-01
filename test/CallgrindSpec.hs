{- | The parser's two load-bearing properties.

__Round-trip exactness.__ Everything this tool does not deliberately change must
come back byte-for-byte, because the whole premise of rewriting a profile is that
the measurement survives it. An early version lifted @totals:@ into its own field
and re-appended it, which produced a file of exactly the same length with two
lines transposed — caught only by comparing the text, not the length.

__Self versus inclusive cost.__ The single cost line after a @calls=@ is the
/inclusive/ cost of that call. Treating it as self cost charges every caller for
its callees, and the error compounds up the tree. The fixture below is built so
that mistake is visible: @caller@ has self cost 10 and calls @callee@ whose
inclusive cost is 500, so a parser that gets it wrong reports 510.
-}
module CallgrindSpec (
  test_roundTrip,
  test_selfCost,
) where

import CodSpeed.Callgrind
import Data.Map.Strict qualified as M
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

{- | Two parts: one benchmark, one metadata dump, with the structural features a
real CodSpeed profile has — object and file lines, a call with its inclusive cost
line, relative positions, and a trailing @totals:@.
-}
fixture :: String
fixture =
  unlines
    [ "# callgrind format"
    , "version: 1"
    , "creator: callgrind-3.26.0.codspeed6"
    , "pid: 4242"
    , "part: 1"
    , ""
    , "desc: Trigger: Client Request: Metadata: haskell-codspeed 0.1.0"
    , "events: Ir Dr"
    , "summary: 0"
    , ""
    , "totals: 0"
    , ""
    , "part: 2"
    , ""
    , "desc: Trigger: Client Request: bench::one"
    , "events: Ir Dr"
    , "summary: 0"
    , ""
    , "ob=/tmp/prog"
    , "fl=Main.hs"
    , "fn=caller"
    , "0 4 1"
    , "+2 6 2"
    , "cob=/lib/libc.so.6"
    , "cfi=libc.c"
    , "cfn=callee"
    , "calls=3 0 "
    , "0 500 200"
    , "fn=callee"
    , "0 500 200"
    , ""
    , "totals: 510 203"
    ]

test_roundTrip :: TestTree
test_roundTrip =
  testGroup
    "parseProfile / renderProfile"
    [ testCase "round-trips byte-for-byte" $
        renderProfile (parseProfile fixture) @?= fixture
    , testCase "finds every part" $
        length (profileParts (parseProfile fixture)) @?= 2
    , testCase "keeps the preamble out of the parts" $
        profilePreamble (parseProfile fixture)
          @?= ["# callgrind format", "version: 1", "creator: callgrind-3.26.0.codspeed6", "pid: 4242"]
    , testCase "reads the trigger of a benchmark part" $
        partTrigger (parts !! 1) @?= Just "bench::one"
    , testCase "reads the trigger of the metadata part too" $
        partTrigger (parts !! 0) @?= Just "Metadata: haskell-codspeed 0.1.0"
    , testCase "keeps totals: reachable" $
        partTotals (parts !! 1) @?= Just "totals: 510 203"
    ]
  where
    parts = profileParts (parseProfile fixture)

test_selfCost :: TestTree
test_selfCost =
  testGroup
    "selfCostByFunction"
    [ -- 4+6 self, and *not* the 500 belonging to the call.
      testCase "sums a function's own cost lines" $
        M.lookup "caller" selfs @?= Just [10, 3]
    , testCase "does not charge a caller for its callee" $
        fmap (take 1) (M.lookup "caller" selfs) @?= Just [10]
    , testCase "the callee keeps its own self cost" $
        M.lookup "callee" selfs @?= Just [500, 200]
    , -- The reconciliation the real profiles satisfy, and the reason to trust
      -- the two rules above on files far too large to check by hand.
      testCase "self costs sum to the recorded totals" $
        foldr addCost [] (M.elems selfs) @?= [510, 203]
    , testCase "a part with no functions has no costs" $
        M.size (selfCostByFunction (profileParts (parseProfile fixture) !! 0)) @?= 0
    ]
  where
    selfs = selfCostByFunction (profileParts (parseProfile fixture) !! 1)
