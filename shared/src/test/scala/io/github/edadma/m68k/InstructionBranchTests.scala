package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Addressing.*

class InstructionBranchTests extends AnyFreeSpec with Matchers with Testing {

  // --- Bcc ----------------------------------------------------------------

  "Bcc with 8-bit displacement when condition true branches" in {
    val cpu = newCPU()
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    cpu.setZ(true)
    new Bcc(Conditional.Equal, 0x10).apply(cpu)
    cpu.PC shouldBe 0x4010
  }

  "Bcc when condition false falls through (PC unchanged)" in {
    val cpu = newCPU()
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    cpu.setZ(false)
    new Bcc(Conditional.Equal, 0x10).apply(cpu)
    cpu.PC shouldBe 0x4000
  }

  "Bcc with disp=0 fetches a 16-bit displacement" in {
    val cpu = newCPU()
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x0100)
    cpu.setZ(true)
    new Bcc(Conditional.Equal, 0).apply(cpu)
    cpu.PC shouldBe (0x4000 + 0x0100)
  }

  "Bcc condition T (always) branches" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    new Bcc(Conditional.True, 0x20).apply(cpu)
    cpu.PC shouldBe 0x4020
  }

  // --- BSR ----------------------------------------------------------------

  "BSR pushes return address and branches" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x1000
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    new BSR(0x10).apply(cpu)
    cpu.PC shouldBe 0x4010
    cpu.SSP shouldBe 0x0FFC
    cpu.memory.readInt(0x0FFC) shouldBe 0x4000
  }

  // --- DBcc ---------------------------------------------------------------

  "DBcc with cond=false decrements and branches if not -1" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x10)  // displacement
    cpu.writeDInt(0, 0x00000005) // count = 5
    new DBcc(Conditional.False, 0).apply(cpu)
    cpu.D(0) shouldBe 0x00000004
    cpu.PC shouldBe 0x4010 // branched (PC at disp word + disp)
  }

  "DBcc with cond=false decrements to -1 falls through" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x10)
    cpu.writeDInt(0, 0x00000000) // → decrements to 0xFFFF (= -1)
    new DBcc(Conditional.False, 0).apply(cpu)
    cpu.D(0) shouldBe 0x0000FFFF
    cpu.PC shouldBe 0x4002 // skipped past disp word
  }

  "DBcc with cond=true skips the disp word and falls through" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.writeDInt(0, 0x05)
    new DBcc(Conditional.True, 0).apply(cpu)
    cpu.D(0) shouldBe 0x05  // not decremented
    cpu.PC shouldBe 0x4002
  }

  // --- Scc ----------------------------------------------------------------

  "Scc T writes 0xFF" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    new Scc(Conditional.True, AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readByte(0x2000) shouldBe 0xFF
  }

  "Scc EQ when Z=0 writes 0" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.memory.writeByte(0x2000, 0xFF) // pre-fill
    cpu.setZ(false)
    new Scc(Conditional.Equal, AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readByte(0x2000) shouldBe 0
  }

  // --- JMP / JSR / RTS / RTR / RTE ---------------------------------------

  "JMP (An)" in {
    val cpu = newCPU()
    cpu.A(0) = 0x5000
    new JMP(AddressRegisterIndirect, 0).apply(cpu)
    cpu.PC shouldBe 0x5000
  }

  "JSR (An) pushes PC then jumps" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x1000
    cpu.PC = 0x4000
    cpu.A(0) = 0x5000
    new JSR(AddressRegisterIndirect, 0).apply(cpu)
    cpu.PC shouldBe 0x5000
    cpu.SSP shouldBe 0x0FFC
    cpu.memory.readInt(0x0FFC) shouldBe 0x4000
  }

  "RTS pops PC" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x0FFC
    cpu.memory.writeInt(0x0FFC, 0x4000)
    RTS.apply(cpu)
    cpu.PC shouldBe 0x4000
    cpu.SSP shouldBe 0x1000
  }

  "RTR pops CCR then PC" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x0FFA
    cpu.memory.writeShort(0x0FFA, 0x001F) // CCR = all set
    cpu.memory.writeInt(0x0FFC, 0x4000)
    RTR.apply(cpu)
    cpu.ccr shouldBe 0x1F
    cpu.PC shouldBe 0x4000
  }

  "RTE pops SR then PC (supervisor only)" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x0FFA
    cpu.memory.writeShort(0x0FFA, 0x2700) // SR = supervisor + IPL=7
    cpu.memory.writeInt(0x0FFC, 0x4000)
    RTE.apply(cpu)
    cpu.sr shouldBe 0x2700
    cpu.PC shouldBe 0x4000
  }
}
