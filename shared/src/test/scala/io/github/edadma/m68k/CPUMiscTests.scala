package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** BCD ops, conditional evaluation, stack push/pop, and exception entry. These don't have width variants in the
  * m68k (BCD is byte-only; conditions are CCR-only) but are tested here in one file for cohesion.
  */
class CPUMiscTests extends AnyFreeSpec with Matchers with Testing {

  // ---- BCD ----------------------------------------------------------------

  "abcd: 23 + 19 = 42" in {
    val cpu = newCPU(); cpu.setX(false)
    cpu.abcd(0x19, 0x23) shouldBe 0x42
    cpu.C shouldBe false; cpu.X shouldBe false
  }

  "abcd: 50 + 50 = 100, C/X set, decimal wrap" in {
    val cpu = newCPU(); cpu.setX(false)
    cpu.abcd(0x50, 0x50) shouldBe 0x00
    cpu.C shouldBe true; cpu.X shouldBe true
  }

  "abcd with X=1 chains" in {
    val cpu = newCPU(); cpu.setX(true)
    cpu.abcd(0x10, 0x20) shouldBe 0x31  // 10 + 20 + 1 carry
    cpu.C shouldBe false
  }

  "sbcd: 50 - 25 = 25" in {
    val cpu = newCPU(); cpu.setX(false)
    cpu.sbcd(0x25, 0x50) shouldBe 0x25
    cpu.C shouldBe false
  }

  "sbcd: 25 - 50 = 75 with borrow" in {
    val cpu = newCPU(); cpu.setX(false)
    cpu.sbcd(0x50, 0x25) shouldBe 0x75
    cpu.C shouldBe true; cpu.X shouldBe true
  }

  "nbcd: -25 = 75 with borrow" in {
    val cpu = newCPU(); cpu.setX(false)
    cpu.nbcd(0x25) shouldBe 0x75
    cpu.C shouldBe true; cpu.X shouldBe true
  }

  "nbcd 0 with X=0 = 0, no borrow" in {
    val cpu = newCPU(); cpu.setX(false)
    cpu.nbcd(0) shouldBe 0
    cpu.C shouldBe false
  }

  // ---- Conditional evaluation --------------------------------------------

  "testcc covers all 16 conditions" in {
    val cpu = newCPU()
    // Set a known CCR: N=1, Z=0, V=0, C=1, X=0
    cpu.setN(true); cpu.setZ(false); cpu.setV(false); cpu.setC(true); cpu.setX(false)

    cpu.testcc(Conditional.True)          shouldBe true
    cpu.testcc(Conditional.False)         shouldBe false
    cpu.testcc(Conditional.High)          shouldBe false   // !C && !Z = false
    cpu.testcc(Conditional.LowSame)       shouldBe true    // C || Z
    cpu.testcc(Conditional.CarryClear)    shouldBe false
    cpu.testcc(Conditional.CarrySet)      shouldBe true
    cpu.testcc(Conditional.NotEqual)      shouldBe true
    cpu.testcc(Conditional.Equal)         shouldBe false
    cpu.testcc(Conditional.OverflowClear) shouldBe true
    cpu.testcc(Conditional.OverflowSet)   shouldBe false
    cpu.testcc(Conditional.Plus)          shouldBe false
    cpu.testcc(Conditional.Minus)         shouldBe true
    cpu.testcc(Conditional.GreaterEqual)  shouldBe false   // N != V
    cpu.testcc(Conditional.LessThan)      shouldBe true    // N != V
    cpu.testcc(Conditional.GreaterThan)   shouldBe false   // (N!=V) || Z is true → !true && false
    cpu.testcc(Conditional.LessEqual)     shouldBe true    // N != V
  }

  "testcc Z=1 makes GT false and LE true regardless of N/V" in {
    val cpu = newCPU()
    cpu.setZ(true); cpu.setN(false); cpu.setV(false)
    cpu.testcc(Conditional.GreaterThan) shouldBe false
    cpu.testcc(Conditional.LessEqual)   shouldBe true
  }

  "testcc N=V (both false) makes GE true" in {
    val cpu = newCPU()
    cpu.setN(false); cpu.setV(false)
    cpu.testcc(Conditional.GreaterEqual) shouldBe true
  }

  "testcc N=V (both true) makes GE true" in {
    val cpu = newCPU()
    cpu.setN(true); cpu.setV(true)
    cpu.testcc(Conditional.GreaterEqual) shouldBe true
  }

  // ---- Stack push/pop ----------------------------------------------------

  "pushShort/popShort round-trip on supervisor stack" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x1000
    cpu.pushShort(0x1234)
    cpu.SSP shouldBe 0x0FFE
    cpu.memory.readShort(0x0FFE) shouldBe 0x1234
    cpu.popShort() shouldBe 0x1234
    cpu.SSP shouldBe 0x1000
  }

  "pushInt/popInt round-trip" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x1000
    cpu.pushInt(0xDEADBEEF)
    cpu.SSP shouldBe 0x0FFC
    cpu.memory.readInt(0x0FFC) shouldBe 0xDEADBEEF
    cpu.popInt() shouldBe 0xDEADBEEF
  }

  // ---- Exception entry ---------------------------------------------------

  "exception pushes SR + PC, switches to supervisor, clears T, vectors PC" in {
    val cpu = newCPU()
    cpu.sr = 0       // user mode, T=0
    cpu.ccr = CCR.X | CCR.C
    cpu.USP = 0x2000 // doesn't matter — we'll be in supervisor after
    cpu.SSP = 0x3000
    cpu.PC = 0x4242
    // Vector at 0x80 (TRAP #0): point at 0x5000
    cpu.memory.writeInt(0x80, 0x5000)
    cpu.memory.writeInt(0x5000, 0xDEADBEEF) // doesn't matter, just to make jumpTo find a region
    cpu.exception(0, 0x80)

    cpu.PC shouldBe 0x5000
    (cpu.sr & SRBit.S) shouldBe SRBit.S
    (cpu.sr & SRBit.T) shouldBe 0
    cpu.SSP shouldBe (0x3000 - 6)  // 4 bytes PC + 2 bytes SR
    cpu.memory.readInt(0x3000 - 4) shouldBe 0x4242
    (cpu.memory.readShort(0x3000 - 6) & 0xFF) shouldBe (CCR.X | CCR.C)
  }

  "exception with level > 0 sets the interrupt mask" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x3000
    cpu.PC = 0x4000
    cpu.memory.writeInt(0x100, 0x6000)
    cpu.exception(5, 0x100)
    ((cpu.sr & SRBit.I) >> SRBit.I_shift) shouldBe 5
  }
}
