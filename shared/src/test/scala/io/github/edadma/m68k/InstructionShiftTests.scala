package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Addressing.*

/** Shift/rotate INSTRUCTION wrappers — register form (3-bit immediate or Dn count) and memory form (always
  * 1-bit, word-wide). Underlying ALU correctness is exhaustively tested in CPUShiftTests; here we just verify
  * the wrapping (count source, register vs. memory dispatch, the count==0 ⇒ 8 quirk).
  */
class InstructionShiftTests extends AnyFreeSpec with Matchers with Testing {

  // count=0 in immediate form means 8
  "ASLByteReg with count immediate 0 means 8" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x01)
    new ASLByteReg(0, 0, 0).apply(cpu) // count=0 (means 8), ir=0 (immediate)
    cpu.D(0) shouldBe 0x00 // 1 << 8 = 256, byte = 0
    cpu.Z shouldBe true
    cpu.C shouldBe true   // bit 7 was the last shifted out (more precisely, bit 0 ended up as carry on shift 8)
    cpu.X shouldBe true
  }

  "ASLShortReg with Dn count" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x0001)
    cpu.writeDInt(1, 4) // count from D1
    new ASLShortReg(1, 1, 0).apply(cpu) // count=D1, ir=1 (Dn count), dreg=D0
    cpu.D(0) shouldBe 0x0010
  }

  "ASRIntReg sign-extends" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x80000000)
    new ASRIntReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0xC0000000
    flags(cpu) shouldBe "n"
  }

  "LSLByteReg basic" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x01)
    new LSLByteReg(3, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0x08
  }

  "LSRShortReg" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x8000)
    new LSRShortReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0x4000
  }

  "ROLByteReg cyclic shift" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x80)
    new ROLByteReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0x01
    cpu.C shouldBe true
  }

  "RORShortReg" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x0001)
    new RORShortReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0x8000
    cpu.C shouldBe true
  }

  "ROXLIntReg with X chains" in {
    val cpu = newCPU()
    cpu.setX(true)
    cpu.writeDInt(0, 0)
    new ROXLIntReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 1   // 0 << 1 with X=1 inserted = 1
    cpu.X shouldBe false
    cpu.C shouldBe false
  }

  "ROXRByteReg" in {
    val cpu = newCPU()
    cpu.setX(false)
    cpu.writeDInt(0, 0x80)
    new ROXRByteReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0x40
  }

  // --- Memory shifts (always 1 bit, word-wide) ---------------------------

  "ASLMem on (An)" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.memory.writeShort(0x2000, 0x0001)
    new ASLMem(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readShort(0x2000) shouldBe 0x0002
  }

  "LSRMem on (An)" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.memory.writeShort(0x2000, 0x0002)
    new LSRMem(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readShort(0x2000) shouldBe 0x0001
  }

  "ROXLMem with X=1" in {
    val cpu = newCPU()
    cpu.setX(true)
    cpu.A(0) = 0x2000
    cpu.memory.writeShort(0x2000, 0)
    new ROXLMem(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readShort(0x2000) shouldBe 1
  }
}
