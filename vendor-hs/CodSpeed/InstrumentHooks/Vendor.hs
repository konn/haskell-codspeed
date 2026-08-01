{- | Marker module for the component that compiles the vendored CodSpeed C library.

Cabal has no way to scope @cc-options@ to a single file, so the vendored
@core.c@ lives in its own private sublibrary and this module exists only to give
that sublibrary something to expose. The Haskell bindings are in
"CodSpeed.Instrument.Raw".
-}
module CodSpeed.InstrumentHooks.Vendor (
  instrumentHooksCommit,
) where

{- | The upstream commit the vendored sources were taken from.

Kept in sync with @vendor\/instrument-hooks\/COMMIT@ by
@scripts\/update-instrument-hooks.sh@.

>>> length instrumentHooksCommit
40
-}
instrumentHooksCommit :: String
instrumentHooksCommit = "1ec92c8c9db59db2d0adc37f47d003d3db4e1c64"
