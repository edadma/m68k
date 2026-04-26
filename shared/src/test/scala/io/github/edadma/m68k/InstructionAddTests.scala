package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Addressing.*

class InstructionAddTests extends AnyFreeSpec with Matchers with Testing {

  // --- ADD Dn ← ea / Dn -----------------------------------------------------

  "ADD.B Dn,Dn" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x55555510)
    cpu.writeDInt(1, 0x66666620)
    new ADDByteToD(0, DataRegisterDirect, 1).apply(cpu)
    cpu.D(0) shouldBe 0x55555530   // upper 24 bits preserved
    flags(cpu) shouldBe ""
  }

  "ADD.W Dn,Dn with carry" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x0000FFFF)
    cpu.writeDInt(1, 0x00000001)
    new ADDShortToD(0, DataRegisterDirect, 1).apply(cpu)
    cpu.D(0) shouldBe 0x00000000   // upper 16 preserved (0)
    flags(cpu) shouldBe "xzc"
  }

  "ADD.L Dn,Dn signed overflow" in {
    val cpu = newCPU()
    cpu.writeDInt(0, Int.MaxValue)
    cpu.writeDInt(1, 1)
    new ADDIntToD(0, DataRegisterDirect, 1).apply(cpu)
    cpu.D(0) shouldBe Int.MinValue
    flags(cpu) shouldBe "nv"
  }

  "ADD.B (An),Dn reads from memory" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.memory.writeByte(0x2000, 0x40)
    cpu.writeDInt(2, 0x00000005)
    new ADDByteToD(2, AddressRegisterIndirect, 0).apply(cpu)
    cpu.D(2) shouldBe 0x00000045
  }

  "ADD.W Dn,(An) writes back to memory (RMW dir=1)" in {
    val cpu = newCPU()
    cpu.A(1) = 0x3000
    cpu.memory.writeShort(0x3000, 0x0010)
    cpu.writeDInt(3, 0x00000020)
    new ADDShortToEA(3, AddressRegisterIndirect, 1).apply(cpu)
    cpu.memory.readShort(0x3000) shouldBe 0x0030
  }

  "ADD.L Dn,(An)+ post-increments" in {
    val cpu = newCPU()
    cpu.A(2) = 0x4000
    cpu.memory.writeInt(0x4000, 0x10000000)
    cpu.writeDInt(0, 0x00000001)
    new ADDIntToEA(0, AddressRegisterIndirectPostincrement, 2).apply(cpu)
    cpu.memory.readInt(0x4000) shouldBe 0x10000001
    cpu.A(2) shouldBe 0x4004
  }

  // --- ADDA — no flag effects, sign-extends source --------------------------

  "ADDA.W with negative source sign-extends" in {
    val cpu = newCPU()
    cpu.A(0) = 0x10000
    cpu.writeDInt(0, 0x0000FFFF) // -1 as a 16-bit
    val before = cpu.ccr
    new ADDAShort(0, DataRegisterDirect, 0).apply(cpu)
    cpu.A(0) shouldBe 0x0FFFF
    cpu.ccr shouldBe before  // ADDA does not touch CCR
  }

  "ADDA.L An,An" in {
    val cpu = newCPU()
    cpu.A(0) = 0x1000
    cpu.A(1) = 0x2000
    new ADDAInt(0, AddressRegisterDirect, 1).apply(cpu)
    cpu.A(0) shouldBe 0x3000
  }

  // --- ADDI -----------------------------------------------------------------

  "ADDI.B #imm,Dn" in {
    val cpu = newCPU()
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x0042) // immediate byte = 0x42 (sign-extended to 0x42)
    cpu.writeDInt(0, 0x10)
    new ADDIByte(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0x52
  }

  "ADDI.L #imm,(An)" in {
    val cpu = newCPU()
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeInt(0x4000, 0x12345678)
    cpu.A(0) = 0x3000
    cpu.memory.writeInt(0x3000, 0x10000000)
    new ADDIInt(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readInt(0x3000) shouldBe 0x22345678
  }

  // --- ADDQ -----------------------------------------------------------------

  "ADDQ.B #4,Dn" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x10)
    new ADDQByte(4, DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0x14
  }

  "ADDQ.L #1,An — touches no flags, full 32-bit add" in {
    val cpu = newCPU()
    cpu.A(0) = 0x10000
    cpu.ccr = 0
    new ADDQInt(1, AddressRegisterDirect, 0).apply(cpu)
    cpu.A(0) shouldBe 0x10001
    cpu.ccr shouldBe 0
  }

  "ADDQ.W #1,An — also no flags, treats as 32-bit (m68k quirk)" in {
    val cpu = newCPU()
    cpu.A(0) = 0xFFFFFFFF
    cpu.ccr = 0
    new ADDQShort(1, AddressRegisterDirect, 0).apply(cpu)
    cpu.A(0) shouldBe 0
    cpu.ccr shouldBe 0
  }

  // --- ADDX -----------------------------------------------------------------

  "ADDX.B Dy,Dx with X chains carry" in {
    val cpu = newCPU()
    cpu.setX(true); cpu.setZ(true)
    cpu.writeDInt(0, 0x000000FF) // Dx
    cpu.writeDInt(1, 0x00000000) // Dy
    new ADDXByte(0, 0, 1).apply(cpu)  // regx=0, rm=0 (Dn,Dn), regy=1
    cpu.D(0) shouldBe 0x00000000
    cpu.X shouldBe true; cpu.C shouldBe true; cpu.Z shouldBe true
  }

  "ADDX.W -(Ay),-(Ax) memory mode predecrements both" in {
    val cpu = newCPU()
    cpu.setX(false); cpu.setZ(true)
    cpu.A(0) = 0x2010 // Ax
    cpu.A(1) = 0x3010 // Ay
    cpu.memory.writeShort(0x300E, 0x1234)
    cpu.memory.writeShort(0x200E, 0x4321)
    new ADDXShort(0, 1, 1).apply(cpu)
    cpu.A(0) shouldBe 0x200E
    cpu.A(1) shouldBe 0x300E
    cpu.memory.readShort(0x200E) shouldBe 0x5555
    cpu.Z shouldBe false
  }
}
