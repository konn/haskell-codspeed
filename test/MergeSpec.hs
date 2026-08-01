{- | The merge's one non-negotiable property, and the traps around it.

__Reconciliation.__ Whatever the merge emits, the self costs of the rewritten
part must sum to exactly what the measured part's did. That is what makes it safe
to hand CodSpeed a profile whose call graph we invented: the number it reports is
still the number Valgrind counted. Integer division cannot be allowed to lose a
unit, which is why the root frame absorbs the remainder.

The rest is arithmetic that is easy to get subtly wrong: inclusive versus self
weight in the folded tree, and recursion turning an edge walk into a loop.
-}
module MergeSpec (
  test_folded,
  test_buckets,
  test_merge,
) where

import CodSpeed.Callgrind
import CodSpeed.Callgrind.Merge
import Data.Map.Strict qualified as M
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

{- | @a@ calls @b@ and @c@; @b@ also has cost of its own.

Self weights: a=1, b=2, c=4. Inclusive: a=7, b=2, c=4.
-}
folded :: String
folded =
  unlines
    [ "a 1"
    , "a;b 2"
    , "a;c 4"
    ]

test_folded :: TestTree
test_folded =
  testGroup
    "parseFolded"
    [ testCase "a path ending at a node is that node's self weight" $
        map ccSelf (parseFolded folded) @?= [1]
    , testCase "children hang off it" $
        map ccName (concatMap ccChildren (parseFolded folded)) @?= ["b", "c"]
    , testCase "the total is inclusive, not the sum of leaves" $
        treeTotal (parseFolded folded) @?= 7
    , testCase "self weight is per name, summed across positions" $
        M.toList (treeSelfByName (parseFolded folded)) @?= [("a", 1), ("b", 2), ("c", 4)]
    , testCase "edges carry a count and the callee's inclusive weight" $
        M.toList (treeEdges (parseFolded folded)) @?= [(("a", "b"), (1, 2)), (("a", "c"), (1, 4))]
    , -- A cost-centre name can reappear deeper in the same path; aggregating the
      -- edges then produces a cycle. Computing over the tree first is what keeps
      -- that from becoming a non-terminating walk.
      testCase "recursion does not loop" $
        treeTotal (parseFolded "x;y;x 5\n") @?= 5
    , testCase "a name repeated in one path is summed once per position" $
        M.lookup "x" (treeSelfByName (parseFolded "x;y;x 5\n")) @?= Just 5
    , testCase "a frame name may contain spaces" $
        map ccName (parseFolded "fused (allocates nothing) 3\n")
          @?= ["fused (allocates nothing)"]
    , testCase "malformed lines are skipped, not fatal" $
        parseFolded "nonsense\n\na 1\n" @?= [CCTree "a" 1 []]
    ]

test_buckets :: TestTree
test_buckets =
  testGroup
    "bucketOf"
    [ testCase "a GHC module symbol is Haskell" $
        bucketOf "ghczminternal_GHCziInternalziNum_zdfNumIntzuzdczp_info" @?= Haskell
    , testCase "the collector is its own bucket" $
        bucketOf "scavenge_block" @?= RtsGC
    , testCase "the nonmoving collector too, wherever the word appears" $
        bucketOf "nonmovingMark" @?= RtsGC
    , -- The reason the GC test runs before the stg_ test: stg_gc_* is stack
      -- overflow handling, and would otherwise be read as collection.
      testCase "thunk and stack machinery is not the collector" $
        bucketOf "stg_upd_frame_info" @?= RtsStack
    , testCase "generic apply is stack machinery" $
        bucketOf "stg_ap_stk_npp" @?= RtsStack
    , testCase "allocation is its own bucket" $
        bucketOf "allocateMightFail" @?= RtsAlloc
    , testCase "libc is not the runtime" $
        bucketOf "__memcpy_avx_unaligned_erms" @?= Libc
    ]

-- | A measured part: 100 units of Haskell, 40 of collector, 60 of stack.
measured :: Part
measured =
  Part
    ["part: 1", "desc: Trigger: Client Request: bench::one", "events: Ir", "summary: 0"]
    [ "fn=ghczminternal_GHCziInternalziNum_zdfNumIntzuzdczp_info"
    , "0 100"
    , "fn=scavenge_block"
    , "0 40"
    , "fn=stg_upd_frame_info"
    , "0 60"
    , ""
    , "totals: 200"
    ]

test_merge :: TestTree
test_merge =
  testGroup
    "mergePart"
    [ testCase "declines when there is no cost-centre profile" $
        mergePart [] measured @?= Nothing
    , testCase "declines on a part with no costs" $
        mergePart (parseFolded folded) (Part [] ["totals: 0"]) @?= Nothing
    , -- The property everything else exists to protect.
      testCase "self costs still sum to the measured total" $
        remerged @?= [200]
    , testCase "the runtime keeps its measured share, unmodelled" $ do
        M.lookup "<RTS: garbage collector>" emitted @?= Just [40]
        M.lookup "<RTS: thunks and stack>" emitted @?= Just [60]
    , -- 100 units of Haskell split by weights 1:2:4 over a total of 7.
      testCase "the Haskell share is split by cost-centre weight" $ do
        M.lookup "a" emitted @?= Just [100 * 1 `div` 7]
        M.lookup "c" emitted @?= Just [100 * 4 `div` 7]
    , testCase "the modelled subtree is labelled in the graph itself" $
        assertBool
          ("expected a labelled frame, got " <> show (M.keys emitted))
          (any (\k -> take 14 k == "<cost centres:") (M.keys emitted))
    , -- Integer division loses units; the root frame takes them so the part
      -- still reconciles. Without this the test above fails by 1.
      testCase "the rounding remainder lands on the root frame" $
        assertBool
          "expected the root frame to carry a non-negative remainder"
          (maybe False ((>= 0) . totalOf) (M.lookup "__codspeed_root_frame__hsBench" emitted))
    ]
  where
    body = maybe [] id (mergePart (parseFolded folded) measured)
    emitted = selfCostByFunction (Part (partHeader measured) body)
    remerged = foldr addCost [] (M.elems emitted)
