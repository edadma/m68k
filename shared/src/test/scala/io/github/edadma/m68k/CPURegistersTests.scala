package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Register-file semantics: byte/short writes preserve the upper bits of Dn; A7 routes to USP/SSP based on
  * supervisor mode; predec/postinc honour the A7 word-alignment quirk for byte-size operations.
  */
class CPURegistersTests extends AnyFreeSpec with Matchers with Testing {

  "writeDByte preserves upper 24 bits" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xAABBCCDD)
    cpu.writeDByte(0, 0x11)
    cpu.D(0) shouldBe 0xAABBCC11
  }

  "writeDShort preserves upper 16 bits" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xAABBCCDD)
    cpu.writeDShort(0, 0x1234)
    cpu.D(0) shouldBe 0xAABB1234
  }

  "writeDInt overwrites all 32 bits" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xAABBCCDD)
    cpu.writeDInt(0, 0x11223344)
    cpu.D(0) shouldBe 0x11223344
  }

  "readDByte sign-extends" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x000000FF)
    cpu.readDByte(0) shouldBe -1
    cpu.writeDInt(0, 0x0000007F)
    cpu.readDByte(0) shouldBe 127
  }

  "readDShort sign-extends" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x0000FFFF)
    cpu.readDShort(0) shouldBe -1
    cpu.writeDInt(0, 0x00007FFF)
    cpu.readDShort(0) shouldBe 32767
  }

  "A7 routes to SSP in supervisor mode and USP in user mode" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.writeA(7, 0x1234)
    cpu.SSP shouldBe 0x1234
    cpu.USP shouldBe 0
    cpu.readA(7) shouldBe 0x1234

    cpu.sr = 0 // user mode
    cpu.writeA(7, 0x5678)
    cpu.USP shouldBe 0x5678
    cpu.SSP shouldBe 0x1234
    cpu.readA(7) shouldBe 0x5678
  }

  "writeA(0..6) targets the An array directly" in {
    val cpu = newCPU()
    for r <- 0 to 6 do {
      cpu.writeA(r, 0x100 + r)
      cpu.A(r) shouldBe (0x100 + r)
      cpu.readA(r) shouldBe (0x100 + r)
    }
  }

  "predecrementA byte size on A7 steps by 2 (alignment quirk)" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x100
    cpu.predecrementA(7, 1) shouldBe 0xFE
    cpu.SSP shouldBe 0xFE
  }

  "predecrementA byte size on A0..A6 steps by 1" in {
    val cpu = newCPU()
    cpu.A(2) = 0x100
    cpu.predecrementA(2, 1) shouldBe 0xFF
    cpu.A(2) shouldBe 0xFF
  }

  "postincrementA byte size on A7 steps by 2" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x100
    cpu.postincrementA(7, 1) shouldBe 0x100 // returns OLD value
    cpu.SSP shouldBe 0x102
  }

  "postincrementA byte size on A0..A6 steps by 1" in {
    val cpu = newCPU()
    cpu.A(3) = 0x100
    cpu.postincrementA(3, 1) shouldBe 0x100
    cpu.A(3) shouldBe 0x101
  }

  "predecrement/postincrement work for short and int" in {
    val cpu = newCPU()
    cpu.A(0) = 0x200
    cpu.predecrementA(0, 2) shouldBe 0x1FE
    cpu.predecrementA(0, 4) shouldBe 0x1FA
    cpu.postincrementA(0, 2) shouldBe 0x1FA
    cpu.A(0) shouldBe 0x1FC
    cpu.postincrementA(0, 4) shouldBe 0x1FC
    cpu.A(0) shouldBe 0x200
  }
}
