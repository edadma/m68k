package io.github.edadma

package object m68k {

  val MAX_ADDRESS   = 0xFFFFFF
  val ADDRESS_RANGE = 0x1000000

  inline def hexByte(a: Int): String    = "%02X".format(a & 0xFF)
  inline def hexShort(a: Int): String   = "%04X".format(a & 0xFFFF)
  inline def hexAddress(a: Int): String = "%06X".format(a & 0xFFFFFF)
  inline def hexInt(a: Int): String     = "%08X".format(a)
  inline def hexLong(a: Long): String   = "%016X".format(a)

  def isHex(s: String): Boolean = s.nonEmpty && s.forall(c => "0123456789abcdefABCDEF" contains c)
  def hex(s: String): Int       = Integer.parseInt(s, 16)

  inline def fromBCD(bcd: Int): Int = (bcd >> 4) * 10 + (bcd & 0x0F)
  inline def toBCD(n: Int): Int     = ((n / 10) << 4) | (n % 10)

  inline def bit(cond: Boolean, bit: Int): Int      = if cond then 1 << bit else 0
  inline def testBit(data: Int, bit: Int): Boolean  = (data & (1 << bit)) != 0
  inline def flipBit(data: Int, bit: Int): Int      = data ^ (1 << bit)
  inline def setBit(data: Int, bit: Int): Int       = data | (1 << bit)
  inline def clearBit(data: Int, bit: Int): Int     = data & ~(1 << bit)

  inline def boolean2int(b: Boolean): Int = if b then 1 else 0

}
