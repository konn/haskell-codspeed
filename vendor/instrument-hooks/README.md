# Vendored `instrument-hooks`

Upstream: <https://github.com/CodSpeedHQ/instrument-hooks>
Pinned commit: see [`COMMIT`](COMMIT).

Only the files this package actually needs are vendored:

| path | why |
| --- | --- |
| `dist/core.c` | the amalgamated (Zig-transpiled) implementation |
| `includes/core.h` | the public API, plus the `static inline` window wrappers |
| `includes/callgrind.h` | Callgrind client-request macros |
| `includes/valgrind.h` | required by `callgrind.h` |
| `includes/compat.h` | required by `core.c` |
| `includes/zig.h` | required by `core.c` |

## Why a committed copy rather than a submodule

`cabal sdist` has no VCS awareness, so a submodule would produce a tarball that cannot build.
A committed copy is the only arrangement where a fresh clone and an unpacked sdist behave alike.

## Updating

```bash
./scripts/update-instrument-hooks.sh          # latest main
./scripts/update-instrument-hooks.sh <sha>    # a specific commit
```

Re-check `cbits/codspeed_shim.c` afterwards: it replicates the bodies of
`instrument_hooks_start_benchmark_inline` / `instrument_hooks_stop_benchmark_inline`, which are
`static inline` in `core.h` and therefore have no linkable symbol of their own. If upstream changes
what those wrappers do, the shim has to change with them.

## Local modifications

None. The files are byte-for-byte upstream, so a diff against the pinned commit stays meaningful.

Compiler warnings are suppressed at the build-system level (see the `instrument-hooks` sublibrary
stanza in `haskell-codspeed.cabal`) rather than by patching the source. Beyond upstream's
documented list we also need `-Wno-incompatible-pointer-types`: `core.c` passes `uintptr_t *` where
`zig.h` declares `uint64_t *`, which is only a mismatch on targets where `uint64_t` is
`unsigned long long` — aarch64-darwin, for instance, but not x86_64-linux.
