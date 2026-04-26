package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Addressing.*

class InstructionLogicTests extends AnyFreeSpec with Matchers with Testing {

  // --- AND -----------------------------------------------------------------

  "AND.B Dn,Dn (dir=0)" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x000000F0)
    cpu.writeDInt(1, 0x0000003F)
    new ANDByteToD(0, DataRegisterDirect, 1).apply(cpu)
    cpu.D(0) shouldBe 0x00000030
    flags(cpu) shouldBe ""
  }

  "AND.W Dn,(An) (dir=1) RMW" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.memory.writeShort(0x2000, 0xF0F0)
    cpu.writeDInt(1, 0x00000F0F)
    new ANDShortToEA(1, AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readShort(0x2000) shouldBe 0x0000
    flags(cpu) shouldBe "z"
  }

  "AND.L touches all 32 bits" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xDEADBEEF)
    cpu.writeDInt(1, 0xFFFFFFFF)
    new ANDIntToD(0, DataRegisterDirect, 1).apply(cpu)
    cpu.D(0) shouldBe 0xDEADBEEF
    flags(cpu) shouldBe "n"
  }

  // --- OR ------------------------------------------------------------------

  "OR.B Dn,Dn" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xF0)
    cpu.writeDInt(1, 0x0F)
    new ORByteToD(0, DataRegisterDirect, 1).apply(cpu)
    cpu.D(0) shouldBe 0xFF
    flags(cpu) shouldBe "n"
  }

  // --- EOR -----------------------------------------------------------------

  "EOR.B Dn,(An) flips bits" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.memory.writeByte(0x2000, 0xF0)
    cpu.writeDInt(1, 0xFF)
    new EORByte(1, AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readByte(0x2000) shouldBe 0x0F
  }

  "EOR.L Dn,Dn = 0 sets Z" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xCAFEBABE)
    cpu.writeDInt(1, 0xCAFEBABE)
    new EORInt(1, DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0
    flags(cpu) shouldBe "z"
  }

  // --- ANDI / ORI / EORI ---------------------------------------------------

  "ANDI.B #imm,Dn" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x000F)
    cpu.writeDInt(0, 0x55)
    new ANDIByte(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0x05
  }

  "ORI.W #imm,(An)" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0xF000)
    cpu.A(0) = 0x2000
    cpu.memory.writeShort(0x2000, 0x0FF0)
    new ORIShort(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readShort(0x2000) shouldBe 0xFFF0
  }

  "EORI.L #imm,Dn" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeInt(0x4000, 0xFFFF0000)
    cpu.writeDInt(0, 0xAAAA5555)
    new EORIInt(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0x55555555
  }

  // --- ANDI/ORI/EORI to CCR (BUG FIX from original) -----------------------

  "ANDI #imm,CCR clears bits where imm has 0 — five-bit mask, no V/C confusion" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x0010) // imm = X bit only
    cpu.ccr = CCR.X | CCR.N | CCR.Z | CCR.V | CCR.C // all set
    ANDItoCCR.apply(cpu)
    cpu.ccr shouldBe CCR.X    // only X survives
  }

  "ORI #imm,CCR sets every bit where imm has 1 — incl C (the original bug)" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x001F)
    cpu.ccr = 0
    ORItoCCR.apply(cpu)
    cpu.ccr shouldBe (CCR.X | CCR.N | CCR.Z | CCR.V | CCR.C)
  }

  "EORI #imm,CCR flips every bit where imm has 1 — incl C (the original bug)" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x001F)
    cpu.ccr = CCR.C   // only C set
    EORItoCCR.apply(cpu)
    cpu.ccr shouldBe (CCR.X | CCR.N | CCR.Z | CCR.V) // C flipped off, others flipped on
  }

  // --- NEG / NEGX / NOT / CLR / TST / EXT / SWAP --------------------------

  "NEG.B (An) on 0x80 sets V and stays 0x80" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.memory.writeByte(0x2000, 0x80)
    new NEGByte(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readByte(0x2000) shouldBe 0x80
    flags(cpu) shouldBe "xnvc"
  }

  "NEGX.L extends from X" in {
    val cpu = newCPU()
    cpu.setX(true); cpu.setZ(true)
    cpu.writeDInt(0, 0)
    new NEGXInt(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe -1
    flags(cpu) shouldBe "xnc"
    cpu.Z shouldBe false
  }

  "NOT.W on Dn" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x0000AAAA)
    new NOTShort(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0x00005555
  }

  "CLR.L Dn writes 0 and sets Z, preserves X" in {
    val cpu = newCPU()
    cpu.setX(true)
    cpu.writeDInt(0, 0xDEADBEEF)
    new CLRInt(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0
    flags(cpu) shouldBe "xz"
  }

  "TST.B (An) sets N for negative byte" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.memory.writeByte(0x2000, 0x80)
    new TSTByte(AddressRegisterIndirect, 0).apply(cpu)
    flags(cpu) shouldBe "n"
  }

  "EXT.W sign-extends low byte into low word" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xFFFF0080) // upper byte should be ignored, low byte 0x80 → -128
    new EXTShort(0).apply(cpu)
    cpu.D(0) shouldBe 0xFFFFFF80
    flags(cpu) shouldBe "n"
  }

  "EXT.L sign-extends low word into full register" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x00008000)
    new EXTInt(0).apply(cpu)
    cpu.D(0) shouldBe 0xFFFF8000
    flags(cpu) shouldBe "n"
  }

  "SWAP exchanges words" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xAABBCCDD)
    new SWAP(0).apply(cpu)
    cpu.D(0) shouldBe 0xCCDDAABB
  }
}
