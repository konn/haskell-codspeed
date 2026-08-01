{- | The tree-shaping logic, tested on hand-built trees.

The suite is a vanilla build, so there is no real cost-centre tree to read here —
that path is exercised by building @example@ the profiling way. What is tested is
everything between the raw tree and the emitted profile, which is where the
judgement calls live.
-}
module CCSSpec (
  test_unavailableWithoutProf,
  test_findById,
  test_diff,
  test_pruning,
  test_folded,
  test_fileNames,
) where

import CodSpeed.Profiling.CCS
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

node :: Int -> String -> String -> Word -> Word -> [CCSNode] -> CCSNode
node i label modName entries alloc children =
  CCSNode
    { ccsId = fromIntegral i
    , ccsLabel = label
    , ccsModule = modName
    , ccsSrcSpan = ""
    , ccsEntries = fromIntegral entries
    , ccsAllocBytes = fromIntegral alloc
    , ccsChildren = children
    }

test_unavailableWithoutProf :: TestTree
test_unavailableWithoutProf =
  testGroup
    "vanilla build"
    [ testCase "reports no cost-centre tree" $ ccsAvailable @?= False
    , testCase "snapshot yields Nothing rather than throwing" $
        snapshotCCS >>= (@?= Nothing)
    , testCase "current stack id yields Nothing" $
        currentCCSId >>= (@?= Nothing)
    ]

test_findById :: TestTree
test_findById =
  testGroup
    "findById"
    [ testCase "finds the root" $
        fmap ccsLabel (findById 1 tree) @?= Just "root"
    , testCase "finds a nested node" $
        fmap ccsLabel (findById 3 tree) @?= Just "deep"
    , testCase "returns Nothing for an unknown id" $
        fmap ccsLabel (findById 99 tree) @?= Nothing
    , -- This is what re-roots a per-benchmark profile at the benchmark.
      testCase "the subtree found keeps its children" $
        fmap (map ccsLabel . ccsChildren) (findById 2 tree) @?= Just ["deep"]
    ]
  where
    tree = node 1 "root" "M" 1 10 [node 2 "mid" "M" 2 20 [node 3 "deep" "M" 3 30 []]]

test_diff :: TestTree
test_diff =
  testGroup
    "diffCCS"
    [ testCase "subtracts counters" $ do
        let before = node 1 "a" "M" 5 100 []
            after = node 1 "a" "M" 8 180 []
            d = diffCCS before after
        (ccsEntries d, ccsAllocBytes d) @?= (3, 80)
    , testCase "matches children by id, not position" $ do
        let before = node 1 "a" "M" 0 0 [node 3 "y" "M" 1 10 [], node 2 "x" "M" 2 20 []]
            after = node 1 "a" "M" 0 0 [node 2 "x" "M" 5 50 [], node 3 "y" "M" 4 40 []]
            d = diffCCS before after
        map (\c -> (ccsLabel c, ccsAllocBytes c)) (ccsChildren d) @?= [("x", 30), ("y", 30)]
    , -- A stack that first appeared during the window owes all of its cost to it.
      testCase "a node absent from the baseline is kept whole" $ do
        let before = node 1 "a" "M" 0 0 []
            after = node 1 "a" "M" 0 0 [node 2 "new" "M" 7 70 []]
        map ccsAllocBytes (ccsChildren (diffCCS before after)) @?= [70]
    , testCase "saturates rather than wrapping on a mismatched pair" $ do
        let d = diffCCS (node 1 "a" "M" 9 90 []) (node 1 "a" "M" 1 10 [])
        (ccsEntries d, ccsAllocBytes d) @?= (0, 0)
    ]

test_pruning :: TestTree
test_pruning =
  testGroup
    "pruneSelfProfiling"
    [ -- Taking the closing snapshot allocates, and that allocation lands in the
      -- diff. Unpruned, a benchmark allocating nothing was reported as spending
      -- 7.5 MB inside the walker.
      testCase "drops the walker's own frames" $
        map ccsLabel (ccsChildren (pruneSelfProfiling withSelf)) @?= ["work"]
    , testCase "prunes at depth too" $
        let nested = node 1 "r" "M" 0 0 [node 2 "w" "M" 0 0 [selfNode]]
            pruned = pruneSelfProfiling nested
         in map (map ccsLabel . ccsChildren) (ccsChildren pruned) @?= [[]]
    , testCase "leaves ordinary frames alone" $
        pruneSelfProfiling clean @?= clean
    ]
  where
    selfNode = node 9 "readNode" "CodSpeed.Profiling.CCS" 1 999 []
    withSelf = node 1 "r" "M" 0 0 [node 2 "work" "M" 1 10 [], selfNode]
    clean = node 1 "r" "M" 0 0 [node 2 "work" "M" 1 10 []]

test_folded :: TestTree
test_folded =
  testGroup
    "foldedStacks"
    [ testCase "renders semicolon-separated paths with a weight" $
        foldedStacks ByAllocation tree
          @?= ["M.root;M.hot 500", "M.root 10", "M.root;M.cold 5"]
    , testCase "weighting by entries reorders" $
        foldedStacks ByEntries tree
          @?= ["M.root;M.cold 90", "M.root;M.hot 20", "M.root 1"]
    , testCase "zero-weight paths are dropped" $
        foldedStacks ByAllocation (node 1 "r" "M" 0 0 [node 2 "z" "M" 0 0 []]) @?= []
    , testCase "a module-less frame renders bare" $
        foldedStacks ByAllocation (node 1 "MAIN" "" 0 8 []) @?= ["MAIN 8"]
    , testGroup
        "stripCommonPrefix"
        [ testCase "drops shared frames, keeping the last as root" $
            stripCommonPrefix [(["a", "b", "c"], 1), (["a", "b", "d"], 2)]
              @?= [(["b", "c"], 1), (["b", "d"], 2)]
        , testCase "a single path is left alone" $
            stripCommonPrefix [(["a", "b"], 1)] @?= [(["a", "b"], 1)]
        , testCase "nothing shared means nothing dropped" $
            stripCommonPrefix [(["a"], 1), (["b"], 2)] @?= [(["a"], 1), (["b"], 2)]
        ]
    , testCase "totals sum the whole tree" $ do
        totalAllocBytes tree @?= 515
        totalEntries tree @?= 111
        length (flatten tree) @?= 3
    ]
  where
    tree =
      node
        1
        "root"
        "M"
        1
        10
        [ node 2 "hot" "M" 20 500 []
        , node 3 "cold" "M" 90 5 []
        ]

test_fileNames :: TestTree
test_fileNames =
  testGroup
    "uriToFileName"
    [ testCase "URI separators become underscores" $
        uriToFileName "bench/Example.hs::sort::1000" @?= "bench_Example.hs__sort__1000"
    , -- Benchmark names are arbitrary text and routinely contain spaces and
      -- punctuation that a filename cannot.
      testCase "spaces and punctuation are replaced" $
        uriToFileName "a b*c?d" @?= "a_b_c_d"
    , testCase "an ordinary name is untouched" $
        uriToFileName "plain-name.1" @?= "plain-name.1"
    , testCase "the result never contains a path separator" $
        assertBool "no slash" ('/' `notElem` uriToFileName "x/y::z")
    ]
