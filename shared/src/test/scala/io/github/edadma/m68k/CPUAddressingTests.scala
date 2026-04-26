package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Addressing.*

/** Effective-address modes: read/write through every addressing mode at every size. The byte-via-Dn case
  * verifies that we stuff the value into bits 0..7 only; the Dn-with-junk-upper-bits case verifies that the
  * sign-extension on read goes through the right number of bits.
  */
class CPUAddressingTests extends AnyFreeSpec with Matchers with Testing {

  "DataRegisterDirect byte read sign-extends and write masks the low byte" in {
    val cpu = newCPU()
    cpu.writeDInt(2, 0xAABBCC80)
    cpu.readByte(DataRegisterDirect, 2) shouldBe -128
    cpu.writeByte(DataRegisterDirect, 2, 0x42)
    cpu.D(2) shouldBe 0xAABBCC42
  }

  "DataRegisterDirect short and int round-trip" in {
    val cpu = newCPU()
    cpu.writeDInt(3, 0x12348000)
    cpu.readShort(DataRegisterDirect, 3) shouldBe -32768
    cpu.writeShort(DataRegisterDirect, 3, 0xCAFE)
    cpu.D(3) shouldBe 0x1234CAFE

    cpu.writeInt(DataRegisterDirect, 3, 0xDEADBEEF)
    cpu.readInt(DataRegisterDirect, 3) shouldBe 0xDEADBEEF
  }

  "AddressRegisterIndirect routes to memory at (An)" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.writeInt(AddressRegisterIndirect, 0, 0xDEADBEEF)
    cpu.memory.readInt(0x2000) shouldBe 0xDEADBEEF
    cpu.readInt(AddressRegisterIndirect, 0) shouldBe 0xDEADBEEF
  }

  "AddressRegisterIndirectPostincrement increments after access" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.writeShort(AddressRegisterIndirectPostincrement, 0, 0x1234)
    cpu.A(0) shouldBe 0x2002
    cpu.memory.readShort(0x2000) shouldBe 0x1234
  }

  "AddressRegisterIndirectPredecrement decrements before access" in {
    val cpu = newCPU()
    cpu.A(1) = 0x2010
    cpu.writeShort(AddressRegisterIndirectPredecrement, 1, 0x9999)
    cpu.A(1) shouldBe 0x200E
    cpu.memory.readShort(0x200E) shouldBe 0x9999
  }

  "AddressRegisterIndirectWithDisplacement consumes a 16-bit signed disp" in {
    val cpu = newCPU()
    // Place a displacement word at PC, then run the EA read
    cpu.A(0) = 0x2000
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x0010) // disp = +16
    cpu.memory.writeInt(0x2010, 0xCAFEBABE)
    cpu.readInt(AddressRegisterIndirectWithDisplacement, 0) shouldBe 0xCAFEBABE
    cpu.PC shouldBe 0x4002 // displacement word consumed
  }

  "AddressRegisterIndirectWithDisplacement honours negative displacement" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2010
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0xFFF0) // disp = -16
    cpu.memory.writeInt(0x2000, 0x12345678)
    cpu.readInt(AddressRegisterIndirectWithDisplacement, 0) shouldBe 0x12345678
  }

  "AbsoluteShort consumes a sign-extended 16-bit address" in {
    val cpu = newCPU()
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x1234)
    cpu.memory.writeInt(0x1234, 0xDEADBEEF)
    cpu.readInt(OtherModes, AbsoluteShort) shouldBe 0xDEADBEEF
  }

  "AbsoluteLong consumes a 32-bit address" in {
    val cpu = newCPU()
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeInt(0x4000, 0x00002000)
    cpu.memory.writeInt(0x2000, 0xCAFEBABE)
    cpu.readInt(OtherModes, AbsoluteLong) shouldBe 0xCAFEBABE
  }

  "ImmediateData byte fetches the low byte of the next word" in {
    val cpu = newCPU()
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x00AB) // immediate byte = 0xAB (sign-extended to -85)
    cpu.readByte(OtherModes, ImmediateData) shouldBe -85
    cpu.PC shouldBe 0x4002
  }

  "ImmediateData short fetches the next word, sign-extended" in {
    val cpu = newCPU()
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x8000)
    cpu.readShort(OtherModes, ImmediateData) shouldBe -32768
  }

  "ImmediateData int fetches the next two words" in {
    val cpu = newCPU()
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeInt(0x4000, 0xDEADBEEF)
    cpu.readInt(OtherModes, ImmediateData) shouldBe 0xDEADBEEF
    cpu.PC shouldBe 0x4004
  }

  "ProgramCounterWithDisplacement is PC-relative" in {
    val cpu = newCPU()
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x0010)  // disp = +16, but EA uses PC AT THE TIME OF THE EXTENSION FETCH
    cpu.memory.writeInt(0x4010, 0xABCDEF01) // PC after fetchShort = 0x4002, but address() captured PC=0x4000
    cpu.readInt(OtherModes, ProgramCounterWithDisplacement) shouldBe 0xABCDEF01
  }

  "AddressRegisterIndirectWithIndex with data-register short index" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.writeDInt(1, 0x00001000) // index Dn (short) = 0x1000
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    // Extension word: D=0 (data reg), reg=1, W (short), disp=+8
    // bits: I/A=0, R=001, W/L=0 (W), 000, disp=08
    // = 0001 0000 0000 1000 = 0x1008
    cpu.memory.writeShort(0x4000, 0x1008)
    cpu.memory.writeInt(0x2000 + 0x1000 + 8, 0xCAFEBABE)
    cpu.readInt(AddressRegisterIndirectWithIndex, 0) shouldBe 0xCAFEBABE
  }
}
