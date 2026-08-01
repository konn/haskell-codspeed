{- | Symbols taken verbatim from a Callgrind profile CI produced, rather than
invented.

The doctests in "CodSpeed.Callgrind.Demangle" cover the shape of the encoding.
These cover the cases that were surprising when the decoder was first run over a
real 460 kB profile, all of which look like bugs until you check them.
-}
module DemangleSpec (
  test_demangle,
) where

import CodSpeed.Callgrind.Demangle (demangleSymbol, zDecode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

test_demangle :: TestTree
test_demangle =
  testGroup
    "demangleSymbol"
    [ testGroup
        "real symbols from a CI profile"
        [ testCase "a worker from the benchmark's own module" $
            demangleSymbol
              "haskellzmcodspeedzm0zi1zi0zi0zminplacezmexample_Main_mainzuzdszdwfuncToBenchLoop_info"
              @?= "Main.main_$s$wfuncToBenchLoop"
        , testCase "the Int addition the leaky fib spends its time in" $
            demangleSymbol "ghczminternal_GHCziInternalziNum_zdfNumIntzuzdczp_info"
              @?= "GHC.Internal.Num.$fNumInt_$c+"
        , testCase "a package with a hash in its unit id" $
            demangleSymbol "basezm4zi21zi2zi0zm60b9_TextziPrintf_zdwadjust_info"
              @?= "Text.Printf.$wadjust"
        , testCase "an operator in the occurrence name" $
            demangleSymbol
              "containerszm0zi7zmeaeb_DataziSequenceziInternal_zdbZCzbzgzuzdssnocTree_info"
              @?= "Data.Sequence.Internal.$b:|>_$ssnocTree"
        , -- _con_info has to beat _info, or the `con` is read as part of the name.
          testCase "a constructor" $
            demangleSymbol "ghczmprim_GHCziTypes_ZC_con_info" @?= "GHC.Types.:"
        ]
    , testGroup
        "left alone"
        [ testCase "an RTS symbol whose second component is lower case" $
            demangleSymbol "stg_ap_v_info" @?= "stg_ap_v_info"
        , testCase "an RTS symbol with an upper-case component" $
            demangleSymbol "stg_BLACKHOLE_info" @?= "stg_BLACKHOLE_info"
        , testCase "our own C shim" $
            demangleSymbol "__codspeed_root_frame__hsBench"
              @?= "__codspeed_root_frame__hsBench"
        , testCase "libc" $
            demangleSymbol "__memcpy_avx_unaligned_erms" @?= "__memcpy_avx_unaligned_erms"
        , testCase "a bare C function with no suffix at all" $
            demangleSymbol "scheduleWaitThread" @?= "scheduleWaitThread"
        ]
    , testGroup
        "Callgrind's own disambiguator"
        [ testCase "is preserved, not decoded" $
            demangleSymbol "basezm4zi21zi2zi0zm60b9_TextziPrintf_toChar_info'2"
              @?= "Text.Printf.toChar'2"
        , testCase "and does not stop an RTS symbol being left alone" $
            demangleSymbol "stg_ap_v_info'2" @?= "stg_ap_v_info'2"
        ]
    , -- This one reads like a half-finished decode and is not. GHC encodes a
      -- stable name by embedding an already-encoded name as data, so the source
      -- carries ZZC and zzm -- and ZZ->Z, zz->z means one decoding pass correctly
      -- yields a literal ZC and zm in the result.
      testCase "a doubly-encoded stable name decodes exactly one level" $
        demangleSymbol
          "haskellzmcodspeedzm0zi1zi0zi0zminplace_CodSpeedziInstrumentziRootFrame_zdfstableZZC0ZZChaskellzzmcodspeedZZCrunActionFromC_info"
          @?= "CodSpeed.Instrument.RootFrame.$fstableZC0ZChaskellzmcodspeedZCrunActionFromC"
    , testGroup
        "zDecode"
        [ testCase "leaves an unencoded string alone" $
            zDecode "balanceL" @?= "balanceL"
        , testCase "decodes the whole escape table" $
            zDecode "zazbzczdzezgzhzizlzmznzpzqzrzsztzuzv"
              @?= "&|^$=>#.<-!+'\\/*_%"
        , testCase "ZZ is a literal Z" $
            zDecode "ZZ" @?= "Z"
        , testCase "zz is a literal z" $
            zDecode "zz" @?= "z"
        ]
    ]
