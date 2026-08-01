{- | Rewrite the symbol names in a Callgrind profile so CodSpeed's flamegraph
frames read as Haskell.

Run after the benchmark and before the runner uploads, which under
@codspeed run@ means chaining it onto the measured command:

@
codspeed run -m simulation -- bash -c './bench && codspeed-hs-rewrite'
@

With no argument it rewrites @$CODSPEED_PROFILE_FOLDER@, which the runner sets.

== Why this is a rename and nothing more

Every cost line is copied through untouched, so totals are preserved by
construction rather than by an assertion that could be wrong. The only lines
altered are @fn=@ and @cfn=@ — the function names Callgrind read out of the ELF
symbol table, which for a GHC binary are z-encoded.

Names are left alone when Callgrind's string compression is on (@fn=(3) name@),
since renaming one occurrence of a compressed name without rewriting the whole id
table would corrupt the file. CodSpeed's runner passes @--compress-strings=no@,
so this is a guard rather than a limitation.

Failure is never fatal: a profile that cannot be rewritten is uploaded as it was,
because a legible flamegraph is worth less than a correct measurement.
-}
module Main (main) where

import CodSpeed.Callgrind.Demangle (demangleSymbol)
import Control.Exception (SomeException, try)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (for_)
import System.Directory (listDirectory, renameFile)
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
          for_ [dir </> n | n <- names, takeExtension n == ".out"] rewriteFile

{- | Rewrite one profile in place, via a temporary file so a failure part-way
leaves the original untouched.
-}
rewriteFile :: FilePath -> IO ()
rewriteFile path = do
  outcome <- try $ do
    contents <- BS.readFile path
    let ls = BS.lines contents
        (renamed, n) = foldr step ([], 0 :: Int) ls
        step l (acc, k) = case rewriteLine l of
          Just l' -> (l' : acc, k + 1)
          Nothing -> (l : acc, k)
    BS.writeFile tmp (BS.unlines renamed)
    renameFile tmp path
    pure n
  case outcome of
    Left err -> warn ("cannot rewrite " <> path) err
    Right 0 ->
      hPutStrLn stderr $
        "[codspeed] rewrite: "
          <> path
          <> ": no GHC symbols found, left unchanged"
    Right n ->
      hPutStrLn stderr $
        "[codspeed] rewrite: " <> path <> ": " <> show n <> " frames renamed"
  where
    tmp = path <> ".rewrite"

{- | 'Just' when the line names a function and the name decoded to something
different.
-}
rewriteLine :: BS.ByteString -> Maybe BS.ByteString
rewriteLine l = do
  (prefix, sym) <- functionLine l
  -- Compressed names are `(3)` or `(3) name`; see the module header.
  if BS.null sym || BS.head sym == '('
    then Nothing
    else do
      let decoded = BS.pack (demangleSymbol (BS.unpack sym))
      if decoded == sym then Nothing else Just (prefix <> decoded)

{- | Split @fn=@ or @cfn=@ off a line, keeping the separator with the prefix.

Deliberately only these two. @fl=@, @fi=@, @fe=@ and @cfi=@ are source files and
@ob=@, @cob=@ are object files; none of them are z-encoded, and rewriting a file
name would only make the profile lie about where the code lives.
-}
functionLine :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
functionLine l
  | Just rest <- BS.stripPrefix (BS.pack "cfn=") l = Just (BS.pack "cfn=", rest)
  | Just rest <- BS.stripPrefix (BS.pack "fn=") l = Just (BS.pack "fn=", rest)
  | otherwise = Nothing

warn :: String -> SomeException -> IO ()
warn what err =
  hPutStrLn stderr $
    "[codspeed] rewrite: " <> what <> ": " <> show err <> " (profile left as-is)"
