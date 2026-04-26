package io.github.edadma.m68k

/** m68k condition codes for Bcc / DBcc / Scc.
  *
  * Indexed 0..15 by the cc field of the opcode. The numeric values are part of the ISA, not just enum tags — opcode
  * decoders rely on `cond` being usable as an array index.
  */
object Conditional {
  val True          = 0  // T
  val False         = 1  // F
  val High          = 2  // HI
  val LowSame       = 3  // LS
  val CarryClear    = 4  // CC / HS
  val CarrySet      = 5  // CS / LO
  val NotEqual      = 6  // NE
  val Equal         = 7  // EQ
  val OverflowClear = 8  // VC
  val OverflowSet   = 9  // VS
  val Plus          = 10 // PL
  val Minus         = 11 // MI
  val GreaterEqual  = 12 // GE
  val LessThan      = 13 // LT
  val GreaterThan   = 14 // GT
  val LessEqual     = 15 // LE

  private val Names = Array(
    "RA", "F", "HI", "LS", "CC", "CS", "NE", "EQ",
    "VC", "VS", "PL", "MI", "GE", "LT", "GT", "LE",
  )

  def apply(cond: Int): String = Names(cond)
}
