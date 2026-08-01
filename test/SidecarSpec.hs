{- | The sidecar is what makes allocation gateable independently of CodSpeed, so
its parser has to survive files that were committed as baselines months earlier
and possibly hand-edited.
-}
module SidecarSpec (
  test_csvRoundTrip,
  test_csvParsing,
  test_comparison,
) where

import CodSpeed.Sidecar
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

rec_ :: String -> Word -> Record
rec_ uri alloc =
  Record
    { recUri = uri
    , recAllocatedBytes = fromIntegral alloc
    , recCopiedBytes = 0
    , recCollections = 0
    , recMajorCollections = 0
    }

test_csvRoundTrip :: TestTree
test_csvRoundTrip =
  testGroup
    "csv round trip"
    [ testCase "empty" $ parseCsv (renderCsv []) @?= Right []
    , testCase "single record" $
        parseCsv (renderCsv [rec_ "a::b" 100]) @?= Right [rec_ "a::b" 100]
    , testCase "output is sorted by uri, so diffs stay stable" $
        parseCsv (renderCsv [rec_ "z" 1, rec_ "a" 2])
          @?= Right [rec_ "a" 2, rec_ "z" 1]
    , -- A tasty benchmark name is arbitrary text and can easily contain a comma.
      testCase "a uri containing a comma survives" $
        parseCsv (renderCsv [rec_ "grp::f(1, 2)" 7]) @?= Right [rec_ "grp::f(1, 2)" 7]
    , testCase "a uri containing a quote survives" $
        parseCsv (renderCsv [rec_ "say \"hi\"" 7]) @?= Right [rec_ "say \"hi\"" 7]
    , testCase "all numeric fields survive" $
        let r = Record "x" 1 2 3 4
         in parseCsv (renderCsv [r]) @?= Right [r]
    ]

test_csvParsing :: TestTree
test_csvParsing =
  testGroup
    "csv parsing"
    [ testCase "a stray blank line is ignored" $
        parseCsv (renderCsv [rec_ "a" 1] <> "\n") @?= Right [rec_ "a" 1]
    , testCase "CRLF input is accepted" $
        parseCsv (concatMap crlf (renderCsv [rec_ "a" 1])) @?= Right [rec_ "a" 1]
    , testCase "a wrong header is rejected rather than mis-parsed" $
        assertBool "expected Left" (isLeft (parseCsv "nope\na,1,2,3,4\n"))
    , testCase "a short row is rejected with its line number" $
        parseCsv (headerOnly <> "bad\n") @?= Left "line 2: expected 5 fields, got 1"
    , testCase "a non-numeric field is rejected by name" $
        parseCsv (headerOnly <> "a,x,0,0,0\n") @?= Left "line 2: bad allocated_bytes: x"
    ]
  where
    crlf '\n' = "\r\n"
    crlf c = [c]
    headerOnly = "uri,allocated_bytes,copied_bytes,collections,major_collections\n"
    isLeft = either (const True) (const False)

test_comparison :: TestTree
test_comparison =
  testGroup
    "comparison"
    [ testCase "a 20% increase is a regression" $
        verdictOf 0.01 [rec_ "a" 1000] [rec_ "a" 1200] @?= Regressed 0.2
    , testCase "a 20% decrease is an improvement" $
        verdictOf 0.01 [rec_ "a" 1000] [rec_ "a" 800] @?= Improved (-0.2)
    , testCase "a change inside the tolerance is unchanged" $
        verdictOf 0.01 [rec_ "a" 1000] [rec_ "a" 1005] @?= Unchanged
    , testCase "identical figures are unchanged even at zero tolerance" $
        verdictOf 0 [rec_ "a" 1000] [rec_ "a" 1000] @?= Unchanged
    , testCase "a new benchmark is reported, not ignored" $
        verdictOf 0.01 [] [rec_ "a" 5] @?= Added
    , -- A benchmark vanishing from the suite is exactly what a gate should catch:
      -- it is otherwise indistinguishable from one that stopped regressing.
      testCase "a vanished benchmark is a regression" $ do
        verdictOf 0.01 [rec_ "a" 5] [] @?= Removed
        length (regressions (compareRuns 0.01 [rec_ "a" 5] [])) @?= 1
    , testCase "growth from zero has no ratio but still counts" $
        verdictOf 0.01 [rec_ "a" 0] [rec_ "a" 100] @?= Regressed 1
    , testCase "zero to zero is unchanged" $
        verdictOf 0.01 [rec_ "a" 0] [rec_ "a" 0] @?= Unchanged
    , testCase "only regressions and removals gate" $
        regressions
          ( compareRuns
              0.01
              [rec_ "up" 100, rec_ "down" 100, rec_ "same" 100]
              [rec_ "up" 200, rec_ "down" 50, rec_ "same" 100]
          )
          @?= [ Comparison "up" (Just 100) (Just 200) (Regressed 1.0)
              ]
    , testCase "results are sorted by uri" $
        map cmpUri (compareRuns 0.01 [rec_ "z" 1, rec_ "a" 1] [rec_ "z" 1, rec_ "a" 1])
          @?= ["a", "z"]
    , testCase "a regression renders with both figures" $
        renderComparison (Comparison "a" (Just 100) (Just 200) (Regressed 1.0))
          @?= "a: +100.0% 100 B -> 200 B"
    ]
  where
    verdictOf tol b c = case compareRuns tol b c of
      (cmp : _) -> cmpVerdict cmp
      [] -> error "compareRuns returned nothing for a non-empty input"
