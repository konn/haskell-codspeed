{- | Turning the symbol names in a Callgrind profile back into Haskell names.

CodSpeed's flamegraph frames come from @VG_(get_fnname)@ on the ELF symbol table,
so a GHC binary renders as z-encoded mangled symbols:

@
ghczminternal_GHCziInternalziNum_zdfNumIntzuzdczp_info
tastyzmbenchzm0zi5zi1zm52572d_TestziTastyziBench_zdwmeasureUntil_info
@

which is the observation that started this package: the flamegraphs show low-level
GHC symbols and syscalls, and nothing you can act on. Decoded, those two are
@GHC.Internal.Num.$fNumInt_$c+@ and @Test.Tasty.Bench.$wmeasureUntil@ — the same
shape the cost-centre profiles use, and legible.

This module does the decoding. Rewriting a profile with it is a pure renaming:
every cost line is untouched, so totals are preserved by construction rather than
by assertion.

== What is deliberately left alone

Symbols that are not GHC module symbols: @stg_ap_stk_npp@, @scheduleWaitThread@,
@__memcpy_avx_unaligned_erms@. They are already as readable as they will get, and
inventing names for them would be worse than leaving the RTS looking like the RTS.
-}
module CodSpeed.Callgrind.Demangle (
  -- * Symbols
  demangleSymbol,

  -- * The underlying encoding
  zDecode,
) where

import Data.Char (chr, digitToInt, isDigit, isHexDigit, isUpper)
import Data.List (intercalate, isSuffixOf)

{- | Rewrite one Callgrind symbol name, if it is a GHC module symbol.

>>> demangleSymbol "ghczminternal_GHCziInternalziNum_zdfNumIntzuzdczp_info"
"GHC.Internal.Num.$fNumInt_$c+"

>>> demangleSymbol "haskellzmcodspeedzm0zi1zi0zi0zminplacezmexample_Main_zdwgo_info"
"Main.$wgo"

Anything that is not one is returned unchanged — the RTS, libc, and our own C
shim:

>>> demangleSymbol "stg_ap_stk_npp"
"stg_ap_stk_npp"

>>> demangleSymbol "__codspeed_root_frame__hsBench"
"__codspeed_root_frame__hsBench"

Callgrind appends @'2@, @'3@ … to disambiguate same-named functions in different
objects. That is Callgrind's, not GHC's, so it is split off before decoding and
put back afterwards — dropping it would merge two frames the profile deliberately
separated.

>>> demangleSymbol "basezm4zi21zi2zi0zm60b9_TextziPrintf_toChar_info'2"
"Text.Printf.toChar'2"
-}
demangleSymbol :: String -> String
demangleSymbol s = case demangleGhc body of
  Just decoded -> decoded <> disambiguator
  Nothing -> s
  where
    (body, disambiguator) = splitDisambiguator s

{- | Split a trailing Callgrind @'N@ off a symbol.

Only digits count, so a Haskell operator encoded with @zq@ (a prime) is not
mistaken for one — that arrives already z-encoded and contains no literal quote.
-}
splitDisambiguator :: String -> (String, String)
splitDisambiguator s = case break (== '\'') s of
  (before, rest@('\'' : ds)) | not (null ds) && all isDigit ds -> (before, rest)
  _ -> (s, "")

{- | @\<unit\>_\<Module\>_\<occ\>_\<suffix\>@ becomes @Module.occ@, decoded.

'Nothing' when the name does not have that shape, which is how RTS and C symbols
survive untouched.
-}
demangleGhc :: String -> Maybe String
demangleGhc sym = do
  stripped <- stripKnownSuffix sym
  case splitOn '_' stripped of
    -- The unit id is dropped: it carries a package hash, and the cost-centre
    -- profiles this is meant to line up with do not use one either.
    (_unit : modl : occ@(_ : _))
      | isModuleName modl ->
          Just (zDecode modl <> "." <> zDecode (intercalate "_" occ))
    _ -> Nothing

{- | Suffixes GHC appends to a symbol.

Longest first: @_con_info@ has to win over @_info@, or the @con@ is mistaken for
part of the name.
-}
knownSuffixes :: [String]
knownSuffixes =
  [ "_con_info"
  , "_static_info"
  , "_con_entry"
  , "_closure"
  , "_entry"
  , "_bytes"
  , "_info"
  , "_slow"
  , "_srt"
  ]

stripKnownSuffix :: String -> Maybe String
stripKnownSuffix sym =
  case [suf | suf <- knownSuffixes, suf `isSuffixOf` sym] of
    (suf : _) -> Just (take (length sym - length suf) sym)
    [] -> Nothing

{- | A z-encoded module name starts with an upper-case letter, since Haskell
module names do. This is what keeps @stg_ap_v_info@ out: strip @_info@ and its
second component is @ap@.
-}
isModuleName :: String -> Bool
isModuleName (c : _) = isUpper c
isModuleName [] = False

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (chunk, []) -> [chunk]
  (chunk, _ : rest) -> chunk : splitOn c rest

{- | GHC's z-encoding, decoded.

Mirrors @zDecodeString@ from @GHC.Utils.Encoding@. Written out rather than taken
from @ghc-boot@ because that package's API tracks the compiler it ships with,
and this is forty lines of a stable, documented encoding.

>>> zDecode "GHCziInternalziNum"
"GHC.Internal.Num"

>>> zDecode "zdwgo"
"$wgo"

>>> zDecode "zdfNumIntzuzdczp"
"$fNumInt_$c+"

Tuples and unboxed tuples have their own forms:

>>> zDecode "Z3T"
"(,,)"

>>> zDecode "Z2H"
"(#,#)"

>>> zDecode "ZLZR"
"()"

Total by construction: an escape this does not recognise is passed through as the
character it already is, because a mangled frame name is a cosmetic problem and
an exception in a profile rewriter is not.

>>> zDecode "zQ"
"Q"
-}
zDecode :: String -> String
zDecode [] = []
zDecode ('Z' : d : rest)
  | isDigit d = decodeTuple (digitToInt d) rest
  | otherwise = decodeUpper d : zDecode rest
zDecode ('z' : d : rest)
  | isDigit d = decodeNumEscape (digitToInt d) rest
  | otherwise = decodeLower d : zDecode rest
zDecode (c : rest) = c : zDecode rest

decodeUpper :: Char -> Char
decodeUpper 'Z' = 'Z'
decodeUpper 'C' = ':'
decodeUpper 'L' = '('
decodeUpper 'R' = ')'
decodeUpper 'M' = '['
decodeUpper 'N' = ']'
decodeUpper c = c

decodeLower :: Char -> Char
decodeLower 'z' = 'z'
decodeLower 'a' = '&'
decodeLower 'b' = '|'
decodeLower 'c' = '^'
decodeLower 'd' = '$'
decodeLower 'e' = '='
decodeLower 'g' = '>'
decodeLower 'h' = '#'
decodeLower 'i' = '.'
decodeLower 'l' = '<'
decodeLower 'm' = '-'
decodeLower 'n' = '!'
decodeLower 'p' = '+'
decodeLower 'q' = '\''
decodeLower 'r' = '\\'
decodeLower 's' = '/'
decodeLower 't' = '*'
decodeLower 'u' = '_'
decodeLower 'v' = '%'
decodeLower c = c

-- | @z\<hex\>U@ — a character by code point.
decodeNumEscape :: Int -> String -> String
decodeNumEscape n (c : rest)
  | isHexDigit c = decodeNumEscape (16 * n + digitToInt c) rest
  | c == 'U' = chr n : zDecode rest
decodeNumEscape n rest = 'z' : show n <> zDecode rest

-- | @Z\<n\>T@ is an @n@-tuple, @Z\<n\>H@ an unboxed one.
decodeTuple :: Int -> String -> String
decodeTuple n (c : rest)
  | isDigit c = decodeTuple (10 * n + digitToInt c) rest
  | c == 'T' =
      (if n == 0 then "()" else '(' : replicate (n - 1) ',' <> ")") <> zDecode rest
  | c == 'H' = '(' : '#' : replicate (n - 1) ',' <> "#)" <> zDecode rest
decodeTuple n rest = 'Z' : show n <> zDecode rest
