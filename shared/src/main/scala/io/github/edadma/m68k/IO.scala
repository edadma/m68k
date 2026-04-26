package io.github.edadma.m68k

/** Memory-mapped I/O devices. Each is one byte wide; the program writes a byte to print, reads a byte to receive
  * input. The host wires these into [[Memory]] at any address it chooses, so a guest binary just needs to know the
  * address.
  *
  * These are line-oriented stdio adapters. Production peripherals (UART, framebuffer, RTC) belong in their own
  * subpackage.
  */
class StdIOChar(val start: Int) extends SingleAddressDevice {
  val name = "stdio-char"
  def readByte(addr: Int): Int               = scala.io.StdIn.readChar().toInt
  def writeByte(addr: Int, value: Int): Unit = print(value.toChar)
}

class StdIOInt(val start: Int) extends SingleAddressDevice {
  val name = "stdio-int"
  def readByte(addr: Int): Int               = scala.io.StdIn.readInt()
  def writeByte(addr: Int, value: Int): Unit = print(value)
}

class StdIOHex(val start: Int) extends SingleAddressDevice {
  val name = "stdio-hex"
  def readByte(addr: Int): Int               = hex(scala.io.StdIn.readLine())
  def writeByte(addr: Int, value: Int): Unit = print(value.toHexString)
}

class RNG(val start: Int) extends ReadOnlyDevice {
  val name = "rng"
  def readByte(addr: Int): Int = util.Random.nextInt(0x100)
}
