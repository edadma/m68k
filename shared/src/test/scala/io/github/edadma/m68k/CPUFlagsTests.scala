package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** CCR / SR plumbing — single-Int flag representation. */
class CPUFlagsTests extends AnyFreeSpec with Matchers with Testing {

  "Individual setX/N/Z/V/C and read-back" in {
    val cpu = newCPU()
    cpu.ccr = 0
    flags(cpu) shouldBe ""

    cpu.setC(true);  flags(cpu) shouldBe "c"
    cpu.setV(true);  flags(cpu) shouldBe "vc"
    cpu.setZ(true);  flags(cpu) shouldBe "zvc"
    cpu.setN(true);  flags(cpu) shouldBe "nzvc"
    cpu.setX(true);  flags(cpu) shouldBe "xnzvc"

    cpu.setN(false); flags(cpu) shouldBe "xzvc"
    cpu.setX(false); flags(cpu) shouldBe "zvc"
  }

  "ccr bit positions match the m68k SR low byte exactly" in {
    val cpu = newCPU()
    cpu.ccr = 0
    cpu.setC(true); (cpu.ccr & 0x01) shouldBe 0x01
    cpu.setV(true); (cpu.ccr & 0x02) shouldBe 0x02
    cpu.setZ(true); (cpu.ccr & 0x04) shouldBe 0x04
    cpu.setN(true); (cpu.ccr & 0x08) shouldBe 0x08
    cpu.setX(true); (cpu.ccr & 0x10) shouldBe 0x10
  }

  "fromSR merges SR high byte and CCR low byte" in {
    val cpu = newCPU()
    cpu.sr  = 0x2700 // supervisor + interrupt mask 7
    cpu.ccr = CCR.X | CCR.C
    cpu.fromSR shouldBe (0x2700 | 0x11)
  }

  "toSR splits high and low" in {
    val cpu = newCPU()
    cpu.toSR(0x2715)
    cpu.sr shouldBe 0x2700
    cpu.ccr shouldBe 0x15
    flags(cpu) shouldBe "xzc" // bits 4 (X), 2 (Z), 0 (C)
  }

  "toCCR only touches the low five bits" in {
    val cpu = newCPU()
    cpu.sr  = 0x8000
    cpu.toCCR(0xFF)
    cpu.sr shouldBe 0x8000
    cpu.ccr shouldBe 0x1F
  }
}
