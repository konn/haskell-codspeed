#!/usr/bin/env bash
# Refresh the vendored CodSpeed instrument-hooks sources.
#
#   ./scripts/update-instrument-hooks.sh          # latest main
#   ./scripts/update-instrument-hooks.sh <sha>    # a specific commit
#
# Fetches the amalgamated implementation and the headers this package needs, and
# records the commit they came from in vendor/instrument-hooks/COMMIT.
#
# Note the destination: upstream ships the amalgamation as dist/core.c, but it is
# vendored under cbits/ here. The repository's .gitignore has an unanchored `dist`
# rule for build output, and it matched the vendored directory too -- the file was
# silently absent from the repository while every local build kept working.
set -euo pipefail

REPO="CodSpeedHQ/instrument-hooks"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEST="$HERE/vendor/instrument-hooks"

REF="${1:-}"
if [ -z "$REF" ]; then
  REF="$(curl -fsSL "https://api.github.com/repos/$REPO/commits/main" \
    | sed -n 's/.*"sha": "\([0-9a-f]\{40\}\)".*/\1/p' | head -1)"
  [ -n "$REF" ] || { echo "could not resolve main" >&2; exit 1; }
fi

echo "updating vendored instrument-hooks to $REF"
mkdir -p "$DEST/cbits" "$DEST/includes"

fetch() { # <upstream path> <local path>
  local url="https://raw.githubusercontent.com/$REPO/$REF/$1"
  curl -fsSL "$url" -o "$DEST/$2" || { echo "  FAILED $1" >&2; exit 1; }
  printf '  %-28s %8d bytes\n' "$2" "$(wc -c <"$DEST/$2")"
}

fetch dist/core.c cbits/core.c
for h in core.h callgrind.h valgrind.h compat.h zig.h; do
  fetch "includes/$h" "includes/$h"
done

echo "$REF" >"$DEST/COMMIT"

# The commit is also compiled in, so it can be reported to CodSpeed as run
# metadata; keeping the two in step matters more than it looks.
VENDOR_HS="$HERE/vendor-hs/CodSpeed/InstrumentHooks/Vendor.hs"
if [ -f "$VENDOR_HS" ]; then
  sed -i.bak "s/^instrumentHooksCommit = \".*\"$/instrumentHooksCommit = \"$REF\"/" "$VENDOR_HS"
  rm -f "$VENDOR_HS.bak"
  echo "  updated instrumentHooksCommit in $(basename "$VENDOR_HS")"
fi

cat <<EOF

Done. Now re-check by hand:

  cbits/codspeed_shim.c replicates the bodies of
  instrument_hooks_{start,stop}_benchmark_inline, which are 'static inline' in
  core.h and so have no linkable symbol. If upstream changed what those wrappers
  do -- particularly which Callgrind client requests they issue -- the shim has
  to change with them.

  cabal build all && cabal test
EOF
