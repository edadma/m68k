package io.github.edadma.m68k

/** m68k Status Register bits (high byte) — supervisor-mode-visible state. */
object SRBit {
  val T       = 0x8000 // trace
  val S       = 0x2000 // supervisor mode
  val I_shift = 8
  val I2      = 0x0400
  val I1      = 0x0200
  val I0      = 0x0100
  val I       = 0x0700 // 3-bit interrupt priority mask
}

/** Condition Code Register bits (SR low byte).
  *
  * Stored as a single Int — five booleans collapsed to one bitfield so flag updates after every ALU op write one
  * masked-OR instead of five branchful boolean stores, and SR push/pop don't have to recombine. The bit positions
  * match the m68k SR low byte exactly, so transferring CCR ↔ SR is a single mask, not a five-field translation.
  */
object CCR {
  val C_bit = 0
  val V_bit = 1
  val Z_bit = 2
  val N_bit = 3
  val X_bit = 4

  val C = 1 << C_bit
  val V = 1 << V_bit
  val Z = 1 << Z_bit
  val N = 1 << N_bit
  val X = 1 << X_bit

  /** Mask of all CCR-visible bits. */
  val Mask = X | N | Z | V | C // 0x1F

  inline def get(ccr: Int, mask: Int): Boolean = (ccr & mask) != 0

  inline def set(ccr: Int, mask: Int, on: Boolean): Int =
    if on then ccr | mask else ccr & ~mask
}
