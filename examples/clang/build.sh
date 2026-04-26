#!/usr/bin/env bash
# Build a freestanding m68k program with clang + m68k-elf-ld and produce a
# flat .bin for the emulator's --bin loader.
#
# clang/LLVM has the M68k backend (experimental); m68k-elf-ld (binutils) does
# the linking because LLD doesn't accept e_machine 4 yet. llvm-objcopy emits
# the final flat binary that --bin can load directly.
#
# The linker script writes the reset vectors (SSP, PC) at addr 0 and places
# .text at 0x0 too — but with .vectors first and .text starting at 0x100. The
# loader synthesises no vectors of its own when --bin is given via build.sh's
# default --load-at 0x0 path; we use --load-at 0x0 so the .bin is loaded at 0
# and the vectors land where the CPU expects them.

set -euo pipefail

LLVM=${LLVM:-$HOME/llvm-project/build/bin}
LD=${LD:-/opt/homebrew/bin/m68k-elf-ld}
SRC=${1:-hello.c}
OUT=${SRC%.c}

"$LLVM/clang" \
    --target=m68k-unknown-linux-gnu \
    -mcpu=M68000 \
    -ffreestanding -nostdlib -fno-pic \
    -O2 \
    -c "$SRC" -o "$OUT.o"

"$LD" -T m68k.ld "$OUT.o" -o "$OUT.elf"

# Flatten everything from the start of the image through the end of .data into
# one binary blob loaded at addr 0. The emulator's --bin --load-at 0 sets up
# nothing — the .vectors section in the blob already supplies SSP and PC.
"$LLVM/llvm-objcopy" -O binary "$OUT.elf" "$OUT.bin"

echo "wrote $OUT.bin ($(wc -c <"$OUT.bin") bytes)"
echo
echo "run with:"
echo "  sbt 'm68kJVM/runMain io.github.edadma.m68k.Main --bin $PWD/$OUT.bin --load-at 0'"
