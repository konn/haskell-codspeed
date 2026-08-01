{- | The shipped defaults, and one assertion that is worth more than it looks.

Reporting @0.1.0.0@ as the integration version — the Cabal version, because
Haskell uses PVP — is enough to make CodSpeed reject the entire run. Not the
benchmark: the run. It arrives as "this run could not be processed" / "No
benchmark results found", with every return code zero, the profile well-formed,
the runner reporting a successful upload and the CI job green.

Switching the single token @0.1.0.0@ to @0.1.0@, changing nothing else, took the
same suite from zero benchmarks recorded to all eight. Every integration CodSpeed
ships reports semver here.

The trap is that the obvious maintenance action — "keep the integration version in
step with the package version" — reintroduces it, and does so silently.
-}
module ConfigSpec (
  test_integrationVersion,
) where

import CodSpeed.Instrument (Integration (..))
import Data.Char (isDigit)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Bench.CodSpeed (Config (..), defaultConfig)
import Test.Tasty.HUnit (assertBool, testCase)

-- | @x.y.z@, exactly three numeric components.
isSemver :: String -> Bool
isSemver s = case splitOn '.' s of
  parts@[_, _, _] -> all numeric parts
  _ -> False
  where
    numeric p = not (null p) && all isDigit p

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (chunk, []) -> [chunk]
  (chunk, _ : rest) -> chunk : splitOn c rest

test_integrationVersion :: TestTree
test_integrationVersion =
  testGroup
    "defaultConfig"
    [ testCase "the integration version is semver, not the PVP package version" $ do
        let v = integrationVersion (configIntegration defaultConfig)
        assertBool
          ( "expected x.y.z, got "
              <> show v
              <> ". A fourth component makes CodSpeed discard the whole run "
              <> "without reporting anything -- see this module's header."
          )
          (isSemver v)
    , testCase "the integration name is non-empty" $ do
        let n = integrationName (configIntegration defaultConfig)
        assertBool "the integration needs a name" (not (null n))
    ]
