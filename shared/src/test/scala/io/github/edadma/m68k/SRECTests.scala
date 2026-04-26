package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SRECTests extends AnyFreeSpec with Matchers {

  "round-trip: write then read recovers the same bytes and start address" in {
    val mem = new Memory {
      def init(): Unit = add(new RAM("scratch", 0x1000, 0x10FF))
    }
    // Build a small ROM, write it as S-records, then parse it back into a fresh memory.
    val src = ROM("orig", 0x100, (0 until 32).map(i => (i * 7).toByte))
    mem.add(src)

    val text = SREC.write(mem, header = "demo".getBytes.toIndexedSeq)

    val mem2 = new Memory {
      def init(): Unit = ()
    }
    val start = SREC.read(mem2, text)
    start shouldBe 0
    for i <- 0 until 32 do
      mem2.readByte(0x100 + i) shouldBe (i * 7) & 0xFF
  }

  "rejects bad checksum" in {
    val mem = new Memory { def init(): Unit = () }
    val bad = "S1060000DEADBE00\n" // checksum should not be 00
    an[Exception] should be thrownBy SREC.read(mem, bad)
  }

  "splits on a non-contiguous gap" in {
    val mem = new Memory { def init(): Unit = () }
    // Two S1 records: one at 0x10 with 4 bytes, another at 0x40 with 4 bytes — not contiguous, so two ROMs.
    val s1a = makeS1(0x0010, Array[Byte](1, 2, 3, 4))
    val s1b = makeS1(0x0040, Array[Byte](5, 6, 7, 8))
    SREC.read(mem, s1a + "\n" + s1b + "\n")
    mem.seqROM.length shouldBe 2
  }

  private def makeS1(addr: Int, data: Array[Byte]): String = {
    val count = data.length + 3
    val sum   = (count + (addr >> 8) + addr + data.foldLeft(0)((s, b) => s + (b & 0xFF))) & 0xFF
    val check = sum ^ 0xFF
    val sb    = new StringBuilder
    sb.append("S1").append(hexByte(count))
    sb.append(hexByte(addr >> 8)).append(hexByte(addr))
    data.foreach(b => sb.append(hexByte(b.toInt)))
    sb.append(hexByte(check))
    sb.toString
  }
}
