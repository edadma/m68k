m68k
====

A Motorola 68000 emulator written in Scala 3, cross-compiled for the JVM, JS, and Native via sbt-crossproject.

The CPU implements the user-mode 68000 ISA — every documented instruction, every addressing mode, every flag side-effect — and includes a default I/O personality (Easy68K-style TRAP #15 services) so freestanding programs can print to the host terminal and exit cleanly.

Programs are loaded as **Motorola S-records** (the canonical m68k toolchain format) or as **flat binaries** (handy for clang/LLVM output, which currently can't be linked with LLD).

Layout
------

```
shared/   cross-platform Scala (CPU, Memory, Instruction, OpcodeTable, SREC, IO, Emulator, ...)
jvm/      JVM entry point (Main) and integration tests that read files from disk
js/       Scala.js platform shim
native/   Scala Native platform shim
examples/ sample programs that build with the new toolchain pipeline
tests/    historical C corpus (.c sources kept, .srec artifacts gitignored)
```

Building and testing
--------------------

```
sbt compile         # all three platforms
sbt m68kJVM/test    # 354 tests, includes file-based integration tests
sbt m68kJS/test     # 350 tests
sbt m68kNative/test # 350 tests
```

Interactive REPL (default)
--------------------------

```
sbt 'm68kJVM/runMain io.github.edadma.m68k.Main'                   # empty REPL
sbt 'm68kJVM/runMain io.github.edadma.m68k.Main path/to/file.srec' # load + REPL
```

Once at the `m68k>` prompt, type `help` for the full command list. Highlights:

- `r [reg val]` — print state, or set a register / SR / CCR / PC
- `u [addr]` / `d [addr]` — disassemble / hex+ASCII dump (each continues from where it left off)
- `s` / `so` / `e [addr [/stop]]` — single step / step-over / run from PC (with optional single-shot breakpoint)
- `b [addr]` / `b -addr` / `b -` — set / clear / clear-all breakpoints
- `t on|off` — trace mode (regs + disasm after every instruction)
- `sy name addr` — define a symbol; addresses can then be referenced by name everywhere
- `m addr byte byte ...` — write bytes to memory; `m` alone prints the memory map

History is persisted to `~/.m68k-repl-history`. ^D exits.

Running non-interactively
-------------------------

```
sbt 'm68kJVM/runMain io.github.edadma.m68k.Main path/to/file.srec --run'
sbt 'm68kJVM/runMain io.github.edadma.m68k.Main --bin path/to/file.bin --load-at 0 --run'
sbt 'm68kJVM/runMain io.github.edadma.m68k.Main path/to/file.srec --run --cycles 100000'
```

`--load-at 0` loads the binary verbatim and trusts its first 8 bytes to be the reset vectors (SSP then PC). For an unwrapped `.text` extract, use `--load-at 0x100` and the loader synthesises vectors at 0 (SSP=0x11000, PC=0x100).

Building m68k programs with clang/LLVM
--------------------------------------

The M68k backend in LLVM is experimental — it's fully functional for codegen but isn't enabled in stock LLVM/clang builds. You need a clang built from source with `LLVM_EXPERIMENTAL_TARGETS_TO_BUILD="M68k"`, plus `m68k-elf-ld` from binutils for linking (LLD doesn't accept `e_machine 4` yet).

On macOS:

```bash
brew install m68k-elf-binutils      # provides m68k-elf-ld, m68k-elf-as, m68k-elf-objcopy
# build LLVM yourself with the M68k experimental target enabled
```

A working example lives in [`examples/clang/`](examples/clang). Build it with:

```bash
cd examples/clang
./build.sh hello.c
sbt 'm68kJVM/runMain io.github.edadma.m68k.Main --bin hello.bin --load-at 0'
```

Output:

```
Hello from clang + m68k-elf-ld!
ABCDEF
```

The build script invokes `clang --target=m68k-unknown-linux-gnu` to produce an ELF object, links with `m68k-elf-ld -T m68k.ld` to fix up cross-section relocations, then `llvm-objcopy -O binary` to flatten the linked image down to a raw blob the emulator can load. Override the toolchain locations with `LLVM=...` and `LD=...`.

TRAP #15 services (Easy68K subset)
----------------------------------

A program invokes a host service by loading a task number into D0 and operands into D1/A1, then executing `TRAP #15`:

| D0 | service                                                  |
|---:|----------------------------------------------------------|
|  0 | print bytes A1..A1+D1.W with trailing newline            |
|  1 | print bytes A1..A1+D1.W (no newline)                     |
|  2 | read a line into the buffer at A1; D1 receives length    |
|  3 | print D1 (decimal)                                       |
|  4 | read a decimal integer into D1                           |
|  5 | read one character into D1                               |
|  6 | print D1.B as a single character                         |
|  9 | stop the CPU                                             |
| 13 | print null-terminated string at A1 with newline          |
| 14 | print null-terminated string at A1 (no newline)          |
| 16 | println D1 (decimal)                                     |

Other tasks (clock, floating-point printing, hex output) are listed in `CPUWithServices.scala`.

Memory map (default)
--------------------

```
0x0000..0x000F   reset vectors (SSP at +0, PC at +4)
0x0010..0xFFFF   ROM — code + read-only data, loaded from SREC/binary
0x10000..0x20FFFF 2 MiB RAM for stack, heap, mutable globals
```

Memory-mapped peripherals (`StdIOChar`, `StdIOInt`, `StdIOHex`, `RNG`) can be added at any address via `Emulator.addDevice(...)`.
