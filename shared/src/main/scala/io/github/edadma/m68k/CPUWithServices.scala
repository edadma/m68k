package io.github.edadma.m68k

/** CPU subclass that wires Easy68K-style TRAP #15 services into the [[CPU.trap]] hook. The guest invokes a service
  * by loading a task number into D0 and the operands into D1/A1, then executing `TRAP #15`.
  *
  * Task numbers (subset — same as the original m68k.io / Easy68K SIM):
  *
  *   0  print bytes A1..A1+D1.W with a trailing newline
  *   1  print bytes A1..A1+D1.W (no newline)
  *   2  read a line into the buffer at A1; D1 receives the length
  *   3  print D1 (decimal)
  *   4  read a decimal integer into D1
  *   5  read one character into D1
  *   6  print D1 as a single character
  *   7  return millisecond clock high/low in D0/D1
  *   8  return centisecond time-of-day in D1
  *   9  stop the CPU
  *  10  print the IEEE 754 double assembled from D1:D2
  *  11  print D1 as unsigned 32-bit
  *  12  print D1 in hexadecimal
  *  13  print null-terminated string at A1 with newline
  *  14  print null-terminated string at A1 (no newline)
  *  15  print the 64-bit integer assembled from D1:D2
  *  16  println D1 (decimal)
  *
  * Anything else falls through to the default vector dispatch.
  */
class CPUWithServices(mem: Memory) extends CPU(mem) {

  override def illegal: Boolean = {
    println("illegal instruction")
    stop()
    true
  }

  override def lineF: Boolean = {
    stop()
    true
  }

  override def trap(vector: Int): Boolean = vector match {
    case 15 => trap15(); true
    case _  => false
  }

  private def trap15(): Unit = {
    def prt(): Unit = {
      val len = D(1) & 0xFFFF
      var i   = 0
      while i < len do {
        print(memory.readByte(A(1) + i).toChar)
        i += 1
      }
    }

    def prtz(): Unit = {
      var addr = A(1)
      var c    = memory.readByte(addr)
      while c != 0 do {
        print(c.toChar)
        addr += 1
        c = memory.readByte(addr)
      }
    }

    (D(0) & 0xFFFF).toShort match {
      case 0 => prt(); println()
      case 1 => prt()
      case 2 =>
        val line = scala.io.StdIn.readLine()
        var i    = 0
        while i < line.length do {
          memory.writeByte(A(1) + i, line.charAt(i).toInt)
          i += 1
        }
        memory.writeByte(A(1) + line.length, 0)
        D(1) = line.length
      case 3 => print(D(1))
      case 4 => D(1) = scala.io.StdIn.readInt()
      case 5 => D(1) = scala.io.StdIn.readChar().toInt
      case 6 => print(D(1).toChar)
      case 7 =>
        val time = System.currentTimeMillis()
        D(0) = (time >> 32).toInt
        D(1) = time.toInt
      case 8 =>
        // Centiseconds since midnight (10ms granularity).
        val nowMillis = System.currentTimeMillis()
        val midnight  = nowMillis - nowMillis % (24L * 60 * 60 * 1000)
        D(1) = ((nowMillis - midnight) / 10).toInt
      case 9 => stop()
      case 10 =>
        val bits = (D(1).toLong << 32) | (D(2) & 0xFFFFFFFFL)
        print(java.lang.Double.longBitsToDouble(bits))
      case 11 => print(D(1).toLong & 0xFFFFFFFFL)
      case 12 => print(D(1).toHexString)
      case 13 => prtz(); println()
      case 14 => prtz()
      case 15 => print((D(1).toLong << 32) | (D(2) & 0xFFFFFFFFL))
      case 16 => println(D(1))
      case n  => sys.error(s"unknown TRAP #15 task: $n")
    }
  }
}
