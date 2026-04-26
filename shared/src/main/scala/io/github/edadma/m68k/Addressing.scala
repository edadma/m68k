package io.github.edadma.m68k

/** m68k effective-address mode constants.
  *
  * The 68000 encodes the EA in two 3-bit fields: `mode` and `reg`. When `mode == OtherModes`, `reg` selects the
  * sub-mode (absolute, PC-relative, immediate). Code reads these values directly out of opcode bits, so they are
  * part of the ISA, not just enum tags.
  */
object Addressing {
  inline val DataRegisterDirect                    = 0
  inline val AddressRegisterDirect                 = 1
  inline val AddressRegisterIndirect               = 2
  inline val AddressRegisterIndirectPostincrement  = 3
  inline val AddressRegisterIndirectPredecrement   = 4
  inline val AddressRegisterIndirectWithDisplacement = 5
  inline val AddressRegisterIndirectWithIndex      = 6
  inline val OtherModes                            = 7

  // Sub-modes selected by `reg` when mode == OtherModes.
  inline val AbsoluteShort                  = 0
  inline val AbsoluteLong                   = 1
  inline val ProgramCounterWithDisplacement = 2
  inline val ProgramCounterWithIndex        = 3
  inline val ImmediateData                  = 4
}
