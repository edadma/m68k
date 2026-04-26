package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** End-to-end tests: assemble a tiny program by writing raw 16-bit opcodes into RAM, run it, check state. This
  * verifies the whole pipeline — opcode dispatch, instruction execution, register/flag/memory side effects —
  * agrees with the per-handler tests above.
  */
class EndToEndTests extends AnyFreeSpec with Matchers with Testing {

  /** Build a CPU with a small ROM-like program at PC=0x1000 and reset vectors that point there. */
  private def withProgram(words: Int*)(setup: CPU => Unit = _ => ()): CPU = {
    val cpu = newCPU()
    cpu.memory.writeInt(0, 0x10000)        // SSP = 0x10000 (top of RAM)
    cpu.memory.writeInt(4, 0x1000)         // PC = 0x1000
    var addr = 0x1000
    for w <- words do {
      cpu.memory.writeShort(addr, w & 0xFFFF)
      addr += 2
    }
    setup(cpu)
    cpu.reset()
    cpu
  }

  "MOVEQ #5,D0 + ADDQ.L #3,D0 leaves 8 in D0" in {
    val cpu = withProgram(
      0x7005, // MOVEQ #5, D0
      0x5680, // ADDQ.L #3, D0  (encoding: 0101 011 0 10 000 000)
    )()
    cpu.step()
    cpu.D(0) shouldBe 5
    cpu.step()
    cpu.D(0) shouldBe 8
  }

  "NOP doesn't change anything but the PC" in {
    val cpu = withProgram(0x4E71)()
    val pcBefore  = cpu.PC
    val ccrBefore = cpu.ccr
    cpu.step()
    cpu.PC shouldBe (pcBefore + 2)
    cpu.ccr shouldBe ccrBefore
  }

  "MOVE.L Dn,Dn" in {
    val cpu = withProgram(
      0x203C, 0xCAFE, 0xBABE, // MOVE.L #$CAFEBABE, D0
      0x2200,                  // MOVE.L D0, D1
    )()
    cpu.step()
    cpu.D(0) shouldBe 0xCAFEBABE
    cpu.step()
    cpu.D(1) shouldBe 0xCAFEBABE
  }

  "Bcc skips a NOP when condition true" in {
    val cpu = withProgram(
      0x6702, // BEQ +2 (skip the next NOP)
      0x4E71, // NOP — should be skipped
      0x4E71, // NOP — falls here after the branch
    )()
    cpu.setZ(true)
    cpu.step() // BEQ
    cpu.PC shouldBe 0x1004 // skipped over the first NOP
    cpu.step() // second NOP
    cpu.PC shouldBe 0x1006
  }

  "ASL.L #1, D0 doubles D0" in {
    val cpu = withProgram(
      0x303C, 0x0042, // MOVE.W #$42, D0  (also clears upper bits to 0)
      0xE388,         // LSL.L #1, D0    (bits: 1110 001 1 10 0 01 000)
    )()
    cpu.step()
    cpu.D(0) shouldBe 0x42
    cpu.step()
    cpu.D(0) shouldBe 0x84
  }

  "Run a SUBQ/BNE counted loop down to 0" in {
    val cpu = withProgram(
      0x303C, 0x0003,         // MOVE.W #3, D0    (counter)
      0x5340,                 // SUBQ.W #1, D0
      0x66FC,                 // BNE -4 (back to SUBQ)
    )()
    cpu.run(cycles = 10)
    cpu.D(0) shouldBe 0  // SUBQ to 0 sets Z, BNE falls through, D0 stays 0
  }
}
