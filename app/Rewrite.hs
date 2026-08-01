{- | Make a Callgrind profile legible to CodSpeed's flamegraph, in two steps.

Run after the benchmark and before the runner uploads, which under
@codspeed run@ means chaining it onto the measured command:

@
codspeed run -m simulation -- bash -c './bench && codspeed-hs-rewrite'
@

With no argument it works on @$CODSPEED_PROFILE_FOLDER@, which the runner sets.

== What it does

__Always: decode the symbols.__ Callgrind reads frame names out of the ELF
symbol table, so a GHC binary renders as
@ghczminternal_GHCziInternalziNum_zdfNumIntzuzdczp_info@. That becomes
@GHC.Internal.Num.$fNumInt_$c+@. A pure rename — every cost line is copied
through untouched.

__When cost-centre profiles are available: merge them in.__ If
@$CODSPEED_HS_CCS_DIR@ holds a @.folded@ profile for a benchmark, that
benchmark's part is rewritten as a cost-centre tree with the measured costs
distributed across it. This is what makes @Main.$wgo@ appear at all: at @-O2@ it
has no ELF frame, and its cost shows up under thunk-entry and update-frame
machinery. See "CodSpeed.Callgrind.Merge" for what is measured and what is
modelled — the distinction matters and the emitted graph is labelled with it.

Both steps preserve every part's @totals:@ exactly, so the number CodSpeed
reports is untouched either way.

Failure is never fatal: a profile that cannot be rewritten is uploaded as it was,
because a legible flamegraph is worth less than a correct measurement.
-}
module Main (main) where

import CodSpeed.Callgrind
import CodSpeed.Callgrind.Demangle (demangleSymbol)
import CodSpeed.Callgrind.Merge (mergePart, parseFolded)
import CodSpeed.Profiling.CCS (uriToFileName)
import Control.Exception (SomeException, evaluate, try)
import Data.List (isPrefixOf, stripPrefix)
import System.Directory (doesFileExist, listDirectory, renameFile)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitSuccess)
import System.FilePath (takeExtension, (</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  folder <- case args of
    (dir : _) -> pure (Just dir)
    [] -> lookupEnv "CODSPEED_PROFILE_FOLDER"
  ccsDir <- lookupEnv "CODSPEED_HS_CCS_DIR"
  case folder of
    Nothing -> do
      hPutStrLn stderr $
        "[codspeed] rewrite: no profile folder. Pass one, or run under "
          <> "`codspeed run`, which sets CODSPEED_PROFILE_FOLDER."
      exitSuccess
    Just dir -> do
      entries <- try (listDirectory dir)
      case entries of
        Left err -> warn ("cannot read " <> dir) err
        Right names ->
          mapM_ (rewriteFile ccsDir) [dir </> n | n <- names, takeExtension n == ".out"]

{- | Rewrite one profile in place, via a temporary file so a failure part-way
leaves the original untouched.
-}
rewriteFile :: Maybe FilePath -> FilePath -> IO ()
rewriteFile ccsDir path = do
  outcome <- try $ do
    profile <- parseProfile <$> readFile path
    parts' <- mapM (rewritePart ccsDir) (profileParts profile)
    let out = renderProfile profile {profileParts = map fst parts'}
    -- Force before opening the handle: readFile is lazy, and writing to the
    -- same path we are still reading would truncate it.
    _ <- evaluate (length out)
    writeFile tmp out
    renameFile tmp path
    pure (length [() | (_, True) <- parts'])
  case outcome of
    Left err -> warn ("cannot rewrite " <> path) err
    Right n ->
      hPutStrLn stderr $
        "[codspeed] rewrite: " <> path <> ": " <> show n <> " part(s) merged with cost centres"
  where
    tmp = path <> ".rewrite"

{- | Rename a part's symbols, and merge cost centres into it when a profile for
its benchmark exists. The 'Bool' says whether a merge happened.
-}
rewritePart :: Maybe FilePath -> Part -> IO (Part, Bool)
rewritePart ccsDir part = do
  merged <- case (ccsDir, partTrigger part) of
    (Just dir, Just uri) | not ("Metadata:" `isPrefixOf` uri) -> do
      let candidate = dir </> uriToFileName uri <> ".folded"
      there <- doesFileExist candidate
      if not there
        then pure Nothing
        else do
          forest <- parseFolded <$> readFile candidate
          pure (mergePart forest part)
    _ -> pure Nothing
  pure $ case merged of
    Just body -> (part {partBody = body}, True)
    Nothing -> (part {partBody = map renameLine (partBody part)}, False)

{- | Decode a @fn=@ or @cfn=@ payload.

Deliberately only those two. @fl=@, @fi=@, @fe=@ and @cfi=@ are source files and
@ob=@, @cob=@ are object files; none are z-encoded, and rewriting a file name
would only make the profile lie about where the code lives.

Names are left alone when Callgrind's string compression is on (@fn=(3) name@),
since renaming one occurrence of a compressed name without rewriting the id table
would corrupt the file. CodSpeed's runner passes @--compress-strings=no@, so this
is a guard rather than a limitation.
-}
renameLine :: String -> String
renameLine l
  | Just sym <- stripPrefix "cfn=" l = "cfn=" <> decode sym
  | Just sym <- stripPrefix "fn=" l = "fn=" <> decode sym
  | otherwise = l
  where
    decode s@('(' : _) = s
    decode s = demangleSymbol s

warn :: String -> SomeException -> IO ()
warn what err =
  hPutStrLn stderr $
    "[codspeed] rewrite: " <> what <> ": " <> show err <> " (profile left as-is)"
