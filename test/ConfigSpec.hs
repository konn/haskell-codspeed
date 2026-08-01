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
import Data.Char (isAlpha, isDigit)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Bench.CodSpeed (Config (..), defaultConfig)
import Test.Tasty.HUnit (assertBool, testCase)

{- | A semver core, plus optional pre-release and build metadata.

Deliberately not a regex copy of the spec's grammar — just enough to catch the
mistake that matters, which is a fourth component in the core.
-}
isSemver :: String -> Bool
isSemver s = case splitOn '.' core of
  parts@[_, _, _] -> all numericNoLeadingZero parts && all validIdents extras
  _ -> False
  where
    -- Build metadata is split off first: it may itself contain a hyphen.
    (beforeBuild, buildPart) = break (== '+') s
    (core, prePart) = break (== '-') beforeBuild
    extras = [drop 1 prePart | not (null prePart)] <> [drop 1 buildPart | not (null buildPart)]

    numericNoLeadingZero p = case p of
      [] -> False
      "0" -> True
      ('0' : _) -> False
      _ -> all isDigit p

    validIdents part =
      let idents = splitOn '.' part
       in not (null idents)
            && all (\i -> not (null i) && all (\c -> isDigit c || isAlpha c || c == '-') i) idents

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
          ( "expected a semver core x.y.z, got "
              <> show v
              <> ". A fourth component in the core makes CodSpeed discard the "
              <> "whole run without reporting anything -- see this module's header."
          )
          (isSemver v)
    , -- Not asserted here: that the string is `pvpToSemver version` rather than a
      -- literal. It cannot be, structurally -- defaultConfig derives it from
      -- Paths_haskell_codspeed -- and pinning it in a test would mean adding an
      -- autogen module to an other-modules field that cabal-gild regenerates.
      testCase "the integration name is non-empty" $ do
        let n = integrationName (configIntegration defaultConfig)
        assertBool "the integration needs a name" (not (null n))
    ]
