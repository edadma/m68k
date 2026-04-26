package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MemoryTests extends AnyFreeSpec with Matchers {

  // Build a memory with three regions on three different pages so the paged
  // dispatch table has to actually steer.
  private def threeRegionMem: Memory = new Memory {
    def init(): Unit = {
      add(new RAM("low", 0x000000, 0x000FFF))   // page 0   (0x000)
      add(new RAM("mid", 0x010000, 0x01FFFF))   // pages 0x10..0x1F
      add(new RAM("high", 0x100000, 0x10FFFF))  // pages 0x100..0x10F
    }
  }

  "Addressable byte/short/int/long readback round-trips" in {
    val r = new RAM("r", 0, 0xFF)
    r.writeByte(0x10, 0xAB)
    r.readByte(0x10) shouldBe 0xAB

    r.writeShort(0x20, 0x1234)
    r.readByte(0x20) shouldBe 0x12 // big-endian
    r.readByte(0x21) shouldBe 0x34
    r.readShort(0x20) shouldBe 0x1234

    r.writeInt(0x30, 0xDEADBEEF)
    r.readInt(0x30) shouldBe 0xDEADBEEF
    r.readShort(0x30) shouldBe 0xDEAD // unsigned 16-bit
    r.readShort(0x32) shouldBe 0xBEEF // unsigned 16-bit (callers sign-extend explicitly)

    r.writeLong(0x40, 0x0123456789ABCDEFL)
    r.readLong(0x40) shouldBe 0x0123456789ABCDEFL
    r.readInt(0x40) shouldBe 0x01234567
    r.readInt(0x44) shouldBe 0x89ABCDEF
  }

  "Memory routes accesses to the owning region" in {
    val m = threeRegionMem
    m.writeByte(0x000050, 0xA1)
    m.writeByte(0x010050, 0xA2)
    m.writeByte(0x100050, 0xA3)
    m.readByte(0x000050) shouldBe 0xA1
    m.readByte(0x010050) shouldBe 0xA2
    m.readByte(0x100050) shouldBe 0xA3
  }

  "Memory routes across page boundaries within a region" in {
    val m = threeRegionMem
    // Last byte of mid region
    m.writeByte(0x01FFFF, 0x77)
    // First byte of mid region
    m.writeByte(0x010000, 0x66)
    m.readByte(0x010000) shouldBe 0x66
    m.readByte(0x01FFFF) shouldBe 0x77
  }

  "Memory short/int writes that span a page boundary inside one region work" in {
    val m = threeRegionMem
    // Page boundary inside "mid" is between 0x010FFF and 0x011000
    m.writeShort(0x010FFF, 0xCAFE)
    m.readByte(0x010FFF) shouldBe 0xCA
    m.readByte(0x011000) shouldBe 0xFE
    m.readShort(0x010FFF) shouldBe 0xCAFE

    m.writeInt(0x011FFE, 0xDEADBEEF)
    m.readInt(0x011FFE) shouldBe 0xDEADBEEF
  }

  "Memory.find raises on an unmapped address" in {
    val m = threeRegionMem
    a[RuntimeException] shouldBe thrownBy(m.readByte(0x005000))
    a[RuntimeException] shouldBe thrownBy(m.readByte(0x500000))
  }

  "Memory.valid distinguishes mapped vs. unmapped" in {
    val m = threeRegionMem
    m.valid(0x000000) shouldBe true
    m.valid(0x000FFF) shouldBe true
    m.valid(0x001000) shouldBe false
    m.valid(0x010800) shouldBe true
    m.valid(0x100FFF) shouldBe true
    m.valid(0x110000) shouldBe false
  }

  "ROM rejects writeByte but accepts programByte" in {
    val rom = ROM("rom", Seq[Byte](0, 0, 0, 0))
    rom.programByte(0, 0x12)
    rom.programByte(1, 0x34)
    rom.readByte(0) shouldBe 0x12
    rom.readByte(1) shouldBe 0x34
    a[RuntimeException] shouldBe thrownBy(rom.writeByte(0, 0xFF))
  }

  "ROM.code helper writes big-endian opcodes" in {
    val rom = ROM.code("rom", 0, Seq(0x4E71, 0x4E73)) // NOP, RTE
    rom.size shouldBe 4
    rom.readShort(0) shouldBe 0x4E71
    rom.readShort(2) shouldBe 0x4E73
  }

  "Memory.add rejects overlapping regions" in {
    val m = new Memory {
      def init(): Unit = add(new RAM("a", 0x1000, 0x1FFF))
    }
    a[RuntimeException] shouldBe thrownBy(m.add(new RAM("b", 0x1800, 0x27FF)))
  }

  "Memory.add rejects duplicate names" in {
    val m = new Memory {
      def init(): Unit = add(new RAM("a", 0x1000, 0x1FFF))
    }
    a[RuntimeException] shouldBe thrownBy(m.add(new RAM("a", 0x4000, 0x4FFF)))
  }

  "Memory.remove takes a region back out and unmaps it" in {
    val m = threeRegionMem
    m.remove("mid")
    m.valid(0x010800) shouldBe false
    m.valid(0x000010) shouldBe true
    m.valid(0x100010) shouldBe true
  }

  "last-region cache: repeated access to the same page hits the cache" in {
    // Indirect test: after one access populates the cache, a second access to
    // the same region must not throw even after a synthetic add() invalidation
    // followed by re-access — semantics must remain correct.
    val m = threeRegionMem
    m.writeByte(0x010040, 0x11)
    m.readByte(0x010040) shouldBe 0x11

    // Add a new region, which invalidates the table and the cache. The next
    // access must still resolve correctly.
    m.add(new RAM("extra", 0x200000, 0x200FFF))
    m.writeByte(0x200000, 0x22)
    m.readByte(0x200000) shouldBe 0x22
    m.readByte(0x010040) shouldBe 0x11
  }

  "seqRAM/seqROM/seqDevice classify regions" in {
    val m = new Memory {
      def init(): Unit = {
        add(new RAM("ram1", 0x0000, 0x0FFF))
        add(ROM("rom1", 0x1000, Seq[Byte](0, 0, 0, 0)))
      }
    }
    m.seqRAM.map(_.name) shouldBe Seq("ram1")
    m.seqROM.map(_.name) shouldBe Seq("rom1")
    m.seqDevice shouldBe empty
  }
}
