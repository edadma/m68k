// Multi-function freestanding m68k program. Now that we have m68k-elf-ld for
// linking, the source can use ordinary C — string literals in .rodata, calls
// between functions — and the linker fixes everything up.
//
// Calls into the host via Easy68K-style TRAP #15 services in CPUWithServices:
//   D0 = 14 → print null-terminated string at A1
//   D0 =  6 → print D1.B as a single character
//   D0 =  9 → stop the CPU

static void puts_(const char *s) {
    register const char *a1 asm("a1") = s;
    register int d0 asm("d0")         = 14;
    asm volatile("trap #15" : "+r"(d0) : "r"(a1) : "memory");
}

static void putc_(int c) {
    register int d1 asm("d1") = c;
    register int d0 asm("d0") = 6;
    asm volatile("trap #15" : "+r"(d0) : "r"(d1) : "memory");
}

static void halt_(void) {
    register int d0 asm("d0") = 9;
    asm volatile("trap #15" : "+r"(d0) : : "memory");
}

__attribute__((noreturn))
void _start(void) {
    puts_("Hello from clang + m68k-elf-ld!\n");

    // Exercise putc_ in a loop so we know separate functions and the call/ret
    // path through the m68k ABI work.
    for (char c = 'A'; c <= 'F'; c++) putc_(c);
    putc_('\n');

    halt_();
    __builtin_unreachable();
}
