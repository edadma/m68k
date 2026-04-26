package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Addressing.*

class InstructionBitTests extends AnyFreeSpec with Matchers with Testing {

  // BCHG / BCLR / BSET / BTST — Dn destination is long-wide; memory is byte-wide.
  // Bit number from Dn (`breg ≥ 0`) or immediate (`breg = -1` ⇒ fetch a word).

  "BCHG #imm,Dn flips a bit and sets Z = !old" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x0010) // bit 16
    cpu.writeDInt(0, 0x00010000)
    new BCHG(-1, DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0
    cpu.Z shouldBe false   // bit was 1
  }

  "BCHG #imm,Dn on a clear bit sets it and Z=true" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x0010)
    cpu.writeDInt(0, 0)
    new BCHG(-1, DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0x00010000
    cpu.Z shouldBe true
  }

  "BCHG Dn,(An) uses bit modulo 8 for memory" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.memory.writeByte(0x2000, 0x00)
    cpu.writeDInt(2, 9) // bit 9 mod 8 = 1
    new BCHG(2, AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readByte(0x2000) shouldBe 0x02
    cpu.Z shouldBe true
  }

  "BCLR clears a set bit, Z reflects original" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xFFFFFFFF)
    cpu.writeDInt(1, 7)
    new BCLR(1, DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0xFFFFFF7F
    cpu.Z shouldBe false
  }

  "BSET sets a clear bit, Z = true (was clear)" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0)
    cpu.writeDInt(1, 5)
    new BSET(1, DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0x20
    cpu.Z shouldBe true
  }

  "BTST tests a bit without modifying" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xFF)
    cpu.writeDInt(1, 7)
    new BTST(1, DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0xFF // unchanged
    cpu.Z shouldBe false   // bit 7 was set
    cpu.writeDInt(1, 8)    // bit 8 (in long Dn) is clear
    new BTST(1, DataRegisterDirect, 0).apply(cpu)
    cpu.Z shouldBe true
  }

  "BTST #imm,(An) memory: only low 3 bits of immediate matter" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.memory.writeByte(0x2000, 0x80)
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x000F) // imm=0x0F → bit 0xF & 7 = 7
    new BTST(-1, AddressRegisterIndirect, 0).apply(cpu)
    cpu.Z shouldBe false
  }

  "TAS sets bit 7, sets Z/N from before-write value" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.memory.writeByte(0x2000, 0x42) // not negative, not zero
    new TAS(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readByte(0x2000) shouldBe 0xC2 // 0x42 | 0x80
    flags(cpu) shouldBe ""
  }

  "TAS on 0 sets Z and the high bit" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.memory.writeByte(0x2000, 0)
    new TAS(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readByte(0x2000) shouldBe 0x80
    cpu.Z shouldBe true
  }
}
