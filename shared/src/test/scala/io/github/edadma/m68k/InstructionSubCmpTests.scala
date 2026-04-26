package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Addressing.*

class InstructionSubCmpTests extends AnyFreeSpec with Matchers with Testing {

  // --- SUB Dn ←/→ ea --------------------------------------------------------

  "SUB.B Dn,Dn (dir=0): Dd := Dd - Ds" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x05)
    cpu.writeDInt(1, 0x10)
    new SUBByteToD(0, DataRegisterDirect, 1).apply(cpu) // Dd=D0, Ds=D1 → D0 := D0 - D1 = 0x05 - 0x10 = 0xF5
    cpu.D(0) shouldBe 0xF5
    flags(cpu) shouldBe "xnc"
  }

  "SUB.W Dn,Dn (dir=0): Dd - Ds" in {
    val cpu = newCPU()
    cpu.writeDInt(2, 0x0010)
    cpu.writeDInt(3, 0x0005)
    new SUBShortToD(2, DataRegisterDirect, 3).apply(cpu) // D2 := 0x10 - 0x05 = 0x0B
    cpu.D(2) shouldBe 0x000B
    flags(cpu) shouldBe ""
  }

  "SUB.L Dn,(An) (dir=1): ea := ea - Dn" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.memory.writeInt(0x2000, 0x100)
    cpu.writeDInt(2, 0x40)
    new SUBIntToEA(2, AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readInt(0x2000) shouldBe 0xC0
  }

  // --- SUBA — no flags, sign-extend short ----------------------------------

  "SUBA.W with negative source sign-extends" in {
    val cpu = newCPU()
    cpu.A(0) = 0x10000
    cpu.writeDInt(1, 0x0000FFFF)
    cpu.ccr = 0
    new SUBAShort(0, DataRegisterDirect, 1).apply(cpu) // A0 := A0 - sign_ext(D1.W) = 0x10000 - (-1) = 0x10001
    cpu.A(0) shouldBe 0x10001
    cpu.ccr shouldBe 0
  }

  // --- SUBI / SUBQ ---------------------------------------------------------

  "SUBI.B #imm,Dn" in {
    val cpu = newCPU()
    cpu.PC = 0x4000
    cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x000F)
    cpu.writeDInt(0, 0x20)
    new SUBIByte(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0x11
  }

  "SUBQ.B #4,Dn" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x10)
    new SUBQByte(4, DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0x0C
  }

  "SUBQ.W #1,An — no flags, full 32-bit" in {
    val cpu = newCPU()
    cpu.A(0) = 0x100
    cpu.ccr = 0
    new SUBQShort(1, AddressRegisterDirect, 0).apply(cpu)
    cpu.A(0) shouldBe 0xFF
    cpu.ccr shouldBe 0
  }

  // --- SUBX ----------------------------------------------------------------

  "SUBX.B Dy,Dx with X=0: Dx := Dx - Dy - 0, sticky Z" in {
    val cpu = newCPU()
    cpu.setX(false); cpu.setZ(true)
    cpu.writeDInt(2, 0x10) // Dx (regy in our naming = destination)
    cpu.writeDInt(3, 0x10) // Dy (regx in our naming = source)
    new SUBXByte(3, 0, 2).apply(cpu)
    cpu.D(2) shouldBe 0x00
    cpu.Z shouldBe true
  }

  "SUBX.B Dy,Dx with X=1: Dx := Dx - Dy - 1, breaks Z" in {
    val cpu = newCPU()
    cpu.setX(true); cpu.setZ(true)
    cpu.writeDInt(2, 0x10)
    cpu.writeDInt(3, 0x10)
    new SUBXByte(3, 0, 2).apply(cpu)
    cpu.D(2) shouldBe 0xFF
    cpu.X shouldBe true; cpu.C shouldBe true; cpu.N shouldBe true; cpu.Z shouldBe false
  }

  // --- CMP -----------------------------------------------------------------

  "CMP.B Dn,Dn equal sets Z" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x42)
    cpu.writeDInt(1, 0x42)
    new CMPByte(0, DataRegisterDirect, 1).apply(cpu)
    flags(cpu) shouldBe "z"
  }

  "CMP.W: 5 cmp 3 has N/C (3 - 5 = -2)" in {
    val cpu = newCPU()
    cpu.writeDInt(2, 0x0003) // Dn
    cpu.writeDInt(3, 0x0005) // ea via DataRegisterDirect on D3
    new CMPShort(2, DataRegisterDirect, 3).apply(cpu) // computes Dn - ea = 3 - 5
    flags(cpu) shouldBe "nc"
  }

  "CMP doesn't touch X or modify Dn" in {
    val cpu = newCPU()
    cpu.setX(true)
    cpu.writeDInt(0, 0x100)
    cpu.writeDInt(1, 0x10)
    new CMPInt(0, DataRegisterDirect, 1).apply(cpu)
    cpu.D(0) shouldBe 0x100   // unchanged
    cpu.X shouldBe true
  }

  "CMPA.W sign-extends source then unsigned-compares" in {
    val cpu = newCPU()
    cpu.A(0) = 0x10000
    cpu.writeDInt(1, 0x0000FFFF) // -1 as W → sign-extended to 0xFFFFFFFF
    new CMPAShort(0, DataRegisterDirect, 1).apply(cpu)
    // 0x10000 - 0xFFFFFFFF: unsigned borrow ⇒ C; result 0x00010001 positive ⇒ no N/Z; no signed overflow
    flags(cpu) shouldBe "c"
  }

  "CMPI.L #imm,Dn" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeInt(0x4000, 100)
    cpu.writeDInt(0, 100)
    new CMPIInt(DataRegisterDirect, 0).apply(cpu)
    flags(cpu) shouldBe "z"
  }

  "CMPM.B (Ay)+,(Ax)+ post-increments both" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000 // Ax (destination)
    cpu.A(1) = 0x3000 // Ay (source)
    cpu.memory.writeByte(0x2000, 0x10)
    cpu.memory.writeByte(0x3000, 0x10)
    new CMPMByte(0, 1).apply(cpu)
    cpu.A(0) shouldBe 0x2001
    cpu.A(1) shouldBe 0x3001
    flags(cpu) shouldBe "z"
  }
}
