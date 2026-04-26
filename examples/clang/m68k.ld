/* Linker script for the m68k emulator (using m68k-elf-ld).
 *
 * Layout matches the emulator's default Memory map:
 *   0x0000..0x0007 reset vectors (SSP, PC) — we generate them right here so
 *                  the SREC/binary already contains the boot info.
 *   0x0100..       code + read-only data, loaded as ROM
 *   0x10000..      RAM (provided by the emulator, not allocated here)
 *
 * SSP is set to 0x11000 — 4 KiB into the emulator's RAM region.
 */

OUTPUT_FORMAT("elf32-m68k")
OUTPUT_ARCH(m68k)
ENTRY(_start)

SECTIONS {
    . = 0x0;
    .vectors : {
        LONG(0x00011000)    /* initial SSP — 4 KiB into RAM */
        LONG(_start)         /* initial PC */
    }

    . = 0x100;
    .text : { *(.text .text.*) }

    .rodata : { *(.rodata .rodata.*) }
    .data   : { *(.data .data.*) }
    .bss    : { *(.bss .bss.*) *(COMMON) }

    /DISCARD/ : {
        *(.comment)
        *(.note .note.*)
        *(.eh_frame .eh_frame.*)
        *(.ARM.*)
    }
}
