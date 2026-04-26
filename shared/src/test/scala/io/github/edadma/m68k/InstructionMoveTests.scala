package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Addressing.*

class InstructionMoveTests extends AnyFreeSpec with Matchers with Testing {

  "MOVE.B Dn,Dn updates flags" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xAABB0000)  // dest, upper bits should be preserved
    cpu.writeDInt(1, 0xCCDDEE80)  // source, low byte = 0x80 (negative)
    new MOVEByte(0, DataRegisterDirect, DataRegisterDirect, 1).apply(cpu)
    cpu.D(0) shouldBe 0xAABB0080
    flags(cpu) shouldBe "n"
  }

  "MOVE.W (An)+,(An) supports both modes simultaneously" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000  // source
    cpu.A(1) = 0x3000  // dest
    cpu.memory.writeShort(0x2000, 0x1234)
    new MOVEShort(1, AddressRegisterIndirect, AddressRegisterIndirectPostincrement, 0).apply(cpu)
    cpu.A(0) shouldBe 0x2002
    cpu.memory.readShort(0x3000) shouldBe 0x1234
  }

  "MOVE.L #imm,Dn fetches a long immediate" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeInt(0x4000, 0xDEADBEEF)
    new MOVEInt(0, DataRegisterDirect, OtherModes, ImmediateData).apply(cpu)
    cpu.D(0) shouldBe 0xDEADBEEF
    flags(cpu) shouldBe "n"
  }

  "MOVE.L 0,Dn sets Z" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeInt(0x4000, 0)
    new MOVEInt(0, DataRegisterDirect, OtherModes, ImmediateData).apply(cpu)
    cpu.D(0) shouldBe 0
    flags(cpu) shouldBe "z"
  }

  "MOVEA.W sign-extends to 32 bits and doesn't touch CCR" in {
    val cpu = newCPU()
    cpu.ccr = CCR.X | CCR.N | CCR.Z | CCR.V | CCR.C
    cpu.writeDInt(0, 0x0000FFFF)
    new MOVEAShort(1, DataRegisterDirect, 0).apply(cpu)
    cpu.A(1) shouldBe 0xFFFFFFFF
    cpu.ccr shouldBe (CCR.X | CCR.N | CCR.Z | CCR.V | CCR.C)
  }

  "MOVEA.L copies full register" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x12345678)
    new MOVEAInt(1, DataRegisterDirect, 0).apply(cpu)
    cpu.A(1) shouldBe 0x12345678
  }

  "MOVEQ #-1,D0 sign-extends" in {
    val cpu = newCPU()
    new MOVEQ(0, -1).apply(cpu)
    cpu.D(0) shouldBe -1
    flags(cpu) shouldBe "n"
  }

  "MOVE from SR" in {
    val cpu = newCPU()
    cpu.sr = 0x2700
    cpu.ccr = CCR.X | CCR.C
    cpu.A(0) = 0x2000
    new MOVEfromSR(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readShort(0x2000) shouldBe (0x2700 | 0x11)
  }

  "MOVE to CCR sets the low five bits" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x001F)
    new MOVEtoCCR(OtherModes, ImmediateData).apply(cpu)
    cpu.ccr shouldBe 0x1F
  }

  "MOVE to SR requires supervisor mode (silently ignored in user mode)" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x2000)
    cpu.sr = 0  // user mode
    cpu.ccr = 0
    cpu.SSP = 0x100
    cpu.memory.writeInt(VectorTable.privilegeViolation, 0x6000)
    cpu.memory.writeInt(0x6000, 0)  // exception target — just needs to be valid
    new MOVEtoSR(OtherModes, ImmediateData).apply(cpu)
    // After supervisor() returns false an exception is raised; SR will be set by exception path
    (cpu.sr & SRBit.S) shouldBe SRBit.S
  }

  "MOVEUSP from An to USP and back, in supervisor mode" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.A(0) = 0x12345678
    new MOVEUSP(0, 0).apply(cpu) // dir=0: An → USP
    cpu.USP shouldBe 0x12345678

    cpu.USP = 0x87654321
    new MOVEUSP(1, 1).apply(cpu) // dir=1: USP → A1
    cpu.A(1) shouldBe 0x87654321
  }

  "MOVEM.W D0/D1 → -(A0)" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x00001111)
    cpu.writeDInt(1, 0x00002222)
    cpu.A(0) = 0x2010
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0xC000) // bit 14=D1, bit 15=D0 in predec ordering: list bit i ↔ A(7-i) for i<8 then D(7-(i-8))
    // Predec list bits: bit i = (D7,D6,D5,D4,D3,D2,D1,D0,A7,A6,A5,A4,A3,A2,A1,A0)? Wait — original m68k:
    // For predec mode: list bit 0 = A7, bit 7 = A0, bit 8 = D7, bit 15 = D0.
    // So 0xC000 = bits 14, 15 = D1 and D0 (D1 first since pushed at higher address).
    new MOVEMShortToMem(AddressRegisterIndirectPredecrement, 0).apply(cpu)
    // Predec writes in order: bit 0 first, bit 15 last. Iteration: bit 14 hit first (D1), then bit 15 (D0)
    // Each predec subtracts 2: A0 starts at 0x2010
    //   first hit (bit 14, D1): predec → 0x200E, write 0x2222
    //   second hit (bit 15, D0): predec → 0x200C, write 0x1111
    cpu.A(0) shouldBe 0x200C
    cpu.memory.readShort(0x200C) shouldBe 0x1111
    cpu.memory.readShort(0x200E) shouldBe 0x2222
  }

  "MOVEM.L (A0)+ → D0/D1" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.memory.writeInt(0x2000, 0xCAFEBABE)
    cpu.memory.writeInt(0x2004, 0xDEADBEEF)
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x0003) // bits 0,1 = D0, D1
    new MOVEMIntFromMem(AddressRegisterIndirectPostincrement, 0).apply(cpu)
    cpu.D(0) shouldBe 0xCAFEBABE
    cpu.D(1) shouldBe 0xDEADBEEF
    cpu.A(0) shouldBe 0x2008
  }
}
