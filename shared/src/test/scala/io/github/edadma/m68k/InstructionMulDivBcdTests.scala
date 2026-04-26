package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Addressing.*

class InstructionMulDivBcdTests extends AnyFreeSpec with Matchers with Testing {

  // --- MULS / MULU --------------------------------------------------------

  "MULS.W signed product" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x00000005)
    cpu.writeDInt(1, 0x0000FFFE) // -2 as W
    new MULS(0, DataRegisterDirect, 1).apply(cpu)
    cpu.D(0) shouldBe -10
    flags(cpu) shouldBe "n"
  }

  "MULS.W zero result sets Z" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0)
    cpu.writeDInt(1, 0x1234)
    new MULS(0, DataRegisterDirect, 1).apply(cpu)
    cpu.D(0) shouldBe 0
    flags(cpu) shouldBe "z"
  }

  "MULU.W unsigned product treats source as unsigned 16-bit" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x0000FFFF)
    cpu.writeDInt(1, 0x00000002)
    new MULU(0, DataRegisterDirect, 1).apply(cpu)
    cpu.D(0) shouldBe 0x0001FFFE
  }

  // --- DIVS / DIVU --------------------------------------------------------

  "DIVS.W: -10 / 3 = -3 r -1" in {
    val cpu = newCPU()
    cpu.writeDInt(0, -10)
    cpu.writeDInt(1, 3)
    new DIVS(0, DataRegisterDirect, 1).apply(cpu)
    val q = (cpu.D(0) << 16) >> 16   // sign-extended low 16
    val r = cpu.D(0) >> 16           // sign-extended high 16
    q shouldBe -3
    r shouldBe -1
    flags(cpu) shouldBe "n"
  }

  "DIVS.W division by zero takes the divide-by-zero exception" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.SSP = 0x1000
    cpu.PC = 0x4000
    cpu.writeDInt(0, 100)
    cpu.writeDInt(1, 0)
    cpu.memory.writeInt(VectorTable.integerDivideByZero, 0x9000)
    new DIVS(0, DataRegisterDirect, 1).apply(cpu)
    cpu.PC shouldBe 0x9000
  }

  "DIVS.W overflow sets V" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x10000000) // huge dividend
    cpu.writeDInt(1, 1)
    new DIVS(0, DataRegisterDirect, 1).apply(cpu)
    cpu.V shouldBe true
  }

  "DIVU.W unsigned" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x00000064) // 100
    cpu.writeDInt(1, 0x00000007) // 7
    new DIVU(0, DataRegisterDirect, 1).apply(cpu)
    val q = cpu.D(0) & 0xFFFF
    val r = (cpu.D(0) >>> 16) & 0xFFFF
    q shouldBe 14
    r shouldBe 2
  }

  "DIVU overflow sets V (quotient > 0xFFFF)" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x10000000)
    cpu.writeDInt(1, 1)
    new DIVU(0, DataRegisterDirect, 1).apply(cpu)
    cpu.V shouldBe true
  }

  // --- ABCD / SBCD / NBCD instruction wrappers ----------------------------

  "ABCD Dn,Dn (r=0)" in {
    val cpu = newCPU()
    cpu.setX(false)
    cpu.writeDInt(0, 0x19) // y/source
    cpu.writeDInt(1, 0x23) // x/destination
    new ABCD(0, 0, 1).apply(cpu)
    cpu.D(1) shouldBe 0x42
  }

  "ABCD -(Ay),-(Ax) memory mode" in {
    val cpu = newCPU()
    cpu.setX(false)
    cpu.A(0) = 0x2010 // Ax (dest)
    cpu.A(1) = 0x3010 // Ay (src)
    cpu.memory.writeByte(0x300F, 0x19)
    cpu.memory.writeByte(0x200F, 0x23)
    new ABCD(1, 1, 0).apply(cpu)
    cpu.memory.readByte(0x200F) shouldBe 0x42
    cpu.A(0) shouldBe 0x200F
    cpu.A(1) shouldBe 0x300F
  }

  "SBCD Dn,Dn" in {
    val cpu = newCPU()
    cpu.setX(false)
    cpu.writeDInt(0, 0x25) // y (subtrahend)
    cpu.writeDInt(1, 0x50) // x (minuend)
    new SBCD(0, 0, 1).apply(cpu)
    cpu.D(1) shouldBe 0x25
  }

  "NBCD ea byte" in {
    val cpu = newCPU()
    cpu.setX(false)
    cpu.A(0) = 0x2000
    cpu.memory.writeByte(0x2000, 0x25)
    new NBCD(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readByte(0x2000) shouldBe 0x75
    cpu.C shouldBe true
  }
}
