package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Addressing.*

class InstructionMiscTests extends AnyFreeSpec with Matchers with Testing {

  "LEA loads address into An without touching memory" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x0010)
    new LEA(1, AddressRegisterIndirectWithDisplacement, 0).apply(cpu)
    cpu.A(1) shouldBe 0x2010
  }

  "PEA pushes the EA onto the stack" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x1000
    cpu.A(0) = 0x12345678
    new PEA(AddressRegisterIndirect, 0).apply(cpu)
    cpu.SSP shouldBe 0x0FFC
    cpu.memory.readInt(0x0FFC) shouldBe 0x12345678
  }

  "LINK An,#disp pushes An, copies SP to An, adds disp to SP" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x1000
    cpu.A(0) = 0xCAFEBABE
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0xFFF0) // disp = -16
    new LINK(0).apply(cpu)
    cpu.SSP shouldBe (0x0FFC - 16) // pushed 4 bytes then SP -= 16
    cpu.A(0) shouldBe 0x0FFC // copied from SP after push
    cpu.memory.readInt(0x0FFC) shouldBe 0xCAFEBABE
  }

  "UNLK An reverses LINK" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x0FE0
    cpu.A(0) = 0x0FFC
    cpu.memory.writeInt(0x0FFC, 0xCAFEBABE)
    new UNLK(0).apply(cpu)
    cpu.SSP shouldBe 0x1000
    cpu.A(0) shouldBe 0xCAFEBABE
  }

  "EXG D0,D1 swaps the registers" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xAA)
    cpu.writeDInt(1, 0xBB)
    new EXG(0, 0x08, 1).apply(cpu)
    cpu.D(0) shouldBe 0xBB
    cpu.D(1) shouldBe 0xAA
  }

  "EXG A0,A1" in {
    val cpu = newCPU()
    cpu.A(0) = 0x1000
    cpu.A(1) = 0x2000
    new EXG(0, 0x09, 1).apply(cpu)
    cpu.A(0) shouldBe 0x2000
    cpu.A(1) shouldBe 0x1000
  }

  "EXG D0,A0" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xAAAA)
    cpu.A(0) = 0x5555
    new EXG(0, 0x11, 0).apply(cpu)
    cpu.D(0) shouldBe 0x5555
    cpu.A(0) shouldBe 0xAAAA
  }

  "TRAP #n vectors via VectorTable.TRAPInstruction + n*4" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x1000
    cpu.PC = 0x4000
    cpu.memory.writeInt(VectorTable.TRAPInstruction + 4 * 5, 0x6000)
    new TRAP(5).apply(cpu)
    cpu.PC shouldBe 0x6000
  }

  "TRAPV with V=1 takes the exception" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x1000
    cpu.PC = 0x4000
    cpu.setV(true)
    cpu.memory.writeInt(VectorTable.TRAPVInstruction, 0x7000)
    TRAPV.apply(cpu)
    cpu.PC shouldBe 0x7000
  }

  "TRAPV with V=0 is a NOP" in {
    val cpu = newCPU()
    cpu.PC = 0x4000
    cpu.setV(false)
    TRAPV.apply(cpu)
    cpu.PC shouldBe 0x4000
  }

  "STOP requires supervisor; in supervisor it loads SR and stops" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x2700)
    STOP.apply(cpu)
    cpu.stopped shouldBe true
    cpu.sr shouldBe 0x2700
  }

  "ILLEGAL takes the illegal-instruction exception" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x1000
    cpu.PC = 0x4000
    cpu.memory.writeInt(VectorTable.illegalInstruction, 0x8000)
    ILLEGAL.apply(cpu)
    cpu.PC shouldBe 0x8000
  }

  "NOP does nothing" in {
    val cpu = newCPU()
    val pcBefore  = cpu.PC
    val ccrBefore = cpu.ccr
    NOP.apply(cpu)
    cpu.PC shouldBe pcBefore
    cpu.ccr shouldBe ccrBefore
  }

  "CHK.W with d in range does nothing" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 5)
    cpu.writeDInt(1, 10)
    new CHKShort(0, DataRegisterDirect, 1).apply(cpu)
    cpu.PC shouldBe 0  // no exception → no PC change
  }

  "CHK.W with d > upper takes exception" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x1000
    cpu.PC = 0x4000
    cpu.writeDInt(0, 20)
    cpu.writeDInt(1, 10)
    cpu.memory.writeInt(VectorTable.CHKInstruction, 0x9000)
    new CHKShort(0, DataRegisterDirect, 1).apply(cpu)
    cpu.PC shouldBe 0x9000
  }
}
