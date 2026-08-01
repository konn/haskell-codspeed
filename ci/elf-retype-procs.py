#!/usr/bin/env python3
"""Retype GHC's module-local procs from STT_NOTYPE to STT_FUNC, in place.

Why this exists
---------------
GHC emits an ELF ``.size`` directive for every proc but a ``.type`` directive only
for externally visible ones (``GHC/CmmToAsm/X86/Ppr.hs``: ``pprTypeDecl`` is gated
on ``externallyVisibleCLabel``). So every module-local top-level proc -- each
``go``, each ``$w`` worker, each lambda-lifted join point -- lands in ``.symtab``
as ``STT_NOTYPE`` with a non-zero ``st_size``.

Valgrind rejects those outright. ``readelf.c`` keeps a symbol only when its type
is one of ``STT_FUNC``, ``STT_OBJECT`` or ``STT_GNU_IFUNC``, so the bulk of a
Haskell binary's ``.text`` is invisible to Callgrind and its cost is reported
against bare hex addresses instead of function names.

Flipping the type nibble is a one-byte edit per symbol in ``.symtab``, which is not
mapped at run time. It changes no instruction and no address, so the measurement is
unaffected -- ``ci/spike-s0-elf-retype.sh`` asserts exactly that by diffing
Callgrind's ``totals:`` before and after.

Only symbols that already carry a size and live in an executable section are
touched. Zero-size symbols are deliberately left alone: those are continuation
labels inside a proc, and promoting them would shatter one function into dozens of
fragments rather than improving attribution.
"""

from __future__ import annotations

import argparse
import struct
import sys

ELFCLASS64 = 2
SHT_SYMTAB = 2
SHF_EXECINSTR = 0x4
STT_NOTYPE = 0
STT_FUNC = 2

# Elf64_Sym: name(4) info(1) other(1) shndx(2) value(8) size(8)
SYM_SIZE = 24
SYM_INFO_OFF = 4
SYM_SHNDX_OFF = 6
SYM_VALUE_OFF = 8
SYM_SIZE_OFF = 16


class NotElf64(Exception):
    pass


def _sections(buf: bytes) -> list[tuple[int, int, int, int, int]]:
    """Return (sh_type, sh_flags, sh_offset, sh_size, sh_entsize) per section."""
    e_shoff = struct.unpack_from("<Q", buf, 0x28)[0]
    e_shentsize, e_shnum = struct.unpack_from("<HH", buf, 0x3A)
    out = []
    for i in range(e_shnum):
        base = e_shoff + i * e_shentsize
        sh_type = struct.unpack_from("<I", buf, base + 4)[0]
        sh_flags = struct.unpack_from("<Q", buf, base + 8)[0]
        sh_offset = struct.unpack_from("<Q", buf, base + 24)[0]
        sh_size = struct.unpack_from("<Q", buf, base + 32)[0]
        sh_entsize = struct.unpack_from("<Q", buf, base + 56)[0]
        out.append((sh_type, sh_flags, sh_offset, sh_size, sh_entsize))
    return out


def retype(buf: bytearray, dry_run: bool = False) -> int:
    """Promote eligible STT_NOTYPE symbols to STT_FUNC. Returns the count."""
    if buf[:4] != b"\x7fELF":
        raise NotElf64("not an ELF file")
    if buf[4] != ELFCLASS64:
        raise NotElf64("only ELF64 is supported (this is ELF32)")

    secs = _sections(buf)
    exec_sections = {i for i, (_, flags, *_rest) in enumerate(secs) if flags & SHF_EXECINSTR}

    promoted = 0
    for sh_type, _flags, sh_offset, sh_size, sh_entsize in secs:
        if sh_type != SHT_SYMTAB:
            continue
        entsize = sh_entsize or SYM_SIZE
        for k in range(sh_size // entsize):
            ent = sh_offset + k * entsize
            info = buf[ent + SYM_INFO_OFF]
            shndx = struct.unpack_from("<H", buf, ent + SYM_SHNDX_OFF)[0]
            value = struct.unpack_from("<Q", buf, ent + SYM_VALUE_OFF)[0]
            size = struct.unpack_from("<Q", buf, ent + SYM_SIZE_OFF)[0]

            if (info & 0xF) != STT_NOTYPE:
                continue
            # A sized symbol at a real address in executable memory is a proc.
            # Anything zero-size is a continuation label; leave it folded in.
            if size == 0 or value == 0 or shndx not in exec_sections:
                continue

            if not dry_run:
                buf[ent + SYM_INFO_OFF] = (info & 0xF0) | STT_FUNC
            promoted += 1

    return promoted


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("binary", help="ELF64 binary to rewrite in place")
    ap.add_argument("-n", "--dry-run", action="store_true", help="report only")
    ap.add_argument("-q", "--quiet", action="store_true")
    args = ap.parse_args()

    with open(args.binary, "rb") as fh:
        buf = bytearray(fh.read())

    try:
        promoted = retype(buf, dry_run=args.dry_run)
    except NotElf64 as exc:
        # Mach-O on darwin, PE on Windows: nothing to do, and not an error.
        if not args.quiet:
            print(f"{args.binary}: skipped ({exc})", file=sys.stderr)
        return 0

    if not args.dry_run and promoted:
        with open(args.binary, "wb") as fh:
            fh.write(buf)

    if not args.quiet:
        verb = "would promote" if args.dry_run else "promoted"
        print(f"{args.binary}: {verb} {promoted} symbols STT_NOTYPE -> STT_FUNC", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
