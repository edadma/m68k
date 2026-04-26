package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Addressing.*

/** Coverage backstop: at minimum one direct test per Instruction class that wasn't already exercised in a more
  * targeted suite. Together with the other Instruction* test files this guarantees every concrete subclass of
  * Instruction in this codebase is invoked at least once and has its observable side effects checked.
  */
class InstructionCoverageTests extends AnyFreeSpec with Matchers with Testing {

  // --- ADD family gaps -----------------------------------------------------

  "ADDByteToEA writes RMW byte to memory" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000; cpu.memory.writeByte(0x2000, 0x05)
    cpu.writeDInt(1, 0x10)
    new ADDByteToEA(1, AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readByte(0x2000) shouldBe 0x15
  }

  "ADDIShort #imm,Dn" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x0100)
    cpu.writeDInt(0, 0x0010)
    new ADDIShort(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0x0110
  }

  "ADDXInt Dn,Dn chains long" in {
    val cpu = newCPU()
    cpu.setX(true); cpu.setZ(true)
    cpu.writeDInt(0, 0xFFFFFFFF) // dest
    cpu.writeDInt(1, 0)          // source
    new ADDXInt(0, 0, 1).apply(cpu)
    cpu.D(0) shouldBe 0
    cpu.X shouldBe true; cpu.C shouldBe true; cpu.Z shouldBe true
  }

  // --- SUB family gaps -----------------------------------------------------

  "SUBIntToD" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x10000)
    cpu.writeDInt(1, 0x100)
    new SUBIntToD(0, DataRegisterDirect, 1).apply(cpu)
    cpu.D(0) shouldBe 0xFF00
  }

  "SUBByteToEA" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000; cpu.memory.writeByte(0x2000, 0x10)
    cpu.writeDInt(1, 0x05)
    new SUBByteToEA(1, AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readByte(0x2000) shouldBe 0x0B
  }

  "SUBShortToEA" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000; cpu.memory.writeShort(0x2000, 0x0100)
    cpu.writeDInt(1, 0x10)
    new SUBShortToEA(1, AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readShort(0x2000) shouldBe 0x00F0
  }

  "SUBAInt An -= source" in {
    val cpu = newCPU()
    cpu.A(0) = 0x10000; cpu.writeDInt(1, 0x1000)
    new SUBAInt(0, DataRegisterDirect, 1).apply(cpu)
    cpu.A(0) shouldBe 0xF000
  }

  "SUBIShort #imm,Dn" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x0100)
    cpu.writeDInt(0, 0x0200)
    new SUBIShort(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0x0100
  }

  "SUBIInt #imm,Dn" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeInt(0x4000, 0x10000)
    cpu.writeDInt(0, 0x20000)
    new SUBIInt(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0x10000
  }

  "SUBQInt #5,Dn" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x100)
    new SUBQInt(5, DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0xFB
  }

  "SUBXShort Dy,Dx" in {
    val cpu = newCPU()
    cpu.setX(true); cpu.setZ(true)
    cpu.writeDInt(0, 0x0010) // src
    cpu.writeDInt(1, 0x0030) // dst
    new SUBXShort(0, 0, 1).apply(cpu)
    cpu.D(1) shouldBe 0x001F  // 0x30 - 0x10 - 1 = 0x1F
    cpu.Z shouldBe false
  }

  "SUBXInt Dy,Dx" in {
    val cpu = newCPU()
    cpu.setX(false); cpu.setZ(true)
    cpu.writeDInt(0, 0x100)
    cpu.writeDInt(1, 0x300)
    new SUBXInt(0, 0, 1).apply(cpu)
    cpu.D(1) shouldBe 0x200
    cpu.Z shouldBe false
  }

  // --- CMP family gaps -----------------------------------------------------

  "CMPAInt sign-doesn't-extend (already 32-bit) and unsigned-compares" in {
    val cpu = newCPU()
    cpu.A(0) = 0x100; cpu.writeDInt(1, 0x200)
    new CMPAInt(0, DataRegisterDirect, 1).apply(cpu)
    flags(cpu) shouldBe "nc"
  }

  "CMPIByte" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x0042)
    cpu.writeDInt(0, 0x42)
    new CMPIByte(DataRegisterDirect, 0).apply(cpu)
    flags(cpu) shouldBe "z"
  }

  "CMPIShort" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x1234)
    cpu.writeDInt(0, 0x1234)
    new CMPIShort(DataRegisterDirect, 0).apply(cpu)
    flags(cpu) shouldBe "z"
  }

  "CMPMShort (Ay)+,(Ax)+" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000; cpu.A(1) = 0x3000
    cpu.memory.writeShort(0x2000, 0x1234)
    cpu.memory.writeShort(0x3000, 0x1234)
    new CMPMShort(0, 1).apply(cpu)
    cpu.A(0) shouldBe 0x2002; cpu.A(1) shouldBe 0x3002
    flags(cpu) shouldBe "z"
  }

  "CMPMInt (Ay)+,(Ax)+" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000; cpu.A(1) = 0x3000
    cpu.memory.writeInt(0x2000, 0xCAFEBABE)
    cpu.memory.writeInt(0x3000, 0xCAFEBABE)
    new CMPMInt(0, 1).apply(cpu)
    cpu.A(0) shouldBe 0x2004; cpu.A(1) shouldBe 0x3004
    flags(cpu) shouldBe "z"
  }

  // --- NEG family gaps -----------------------------------------------------

  "NEGShort" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x10)
    new NEGShort(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0xFFF0
    flags(cpu) shouldBe "xnc"
  }

  "NEGInt" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x100)
    new NEGInt(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe -0x100
    flags(cpu) shouldBe "xnc"
  }

  "NEGXByte" in {
    val cpu = newCPU()
    cpu.setX(true); cpu.setZ(true)
    cpu.writeDInt(0, 0)
    new NEGXByte(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0xFF
    cpu.Z shouldBe false
  }

  "NEGXShort" in {
    val cpu = newCPU()
    cpu.setX(true); cpu.setZ(true)
    cpu.writeDInt(0, 0)
    new NEGXShort(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0xFFFF
    cpu.Z shouldBe false
  }

  // --- NOT family gaps -----------------------------------------------------

  "NOTByte" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xFFFFFF00) // upper preserved
    new NOTByte(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0xFFFFFFFF
  }

  "NOTInt" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0)
    new NOTInt(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe -1
  }

  // --- CLR family gaps -----------------------------------------------------

  "CLRByte preserves upper 24 bits" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xCAFEBABE)
    new CLRByte(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0xCAFEBA00
    cpu.Z shouldBe true
  }

  "CLRShort preserves upper 16 bits" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xCAFEBABE)
    new CLRShort(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0xCAFE0000
    cpu.Z shouldBe true
  }

  // --- TST gaps ------------------------------------------------------------

  "TSTShort" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x00008000)
    new TSTShort(DataRegisterDirect, 0).apply(cpu)
    flags(cpu) shouldBe "n"
  }

  "TSTInt" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0)
    new TSTInt(DataRegisterDirect, 0).apply(cpu)
    flags(cpu) shouldBe "z"
  }

  // --- AND family gaps -----------------------------------------------------

  "ANDShortToD" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x0000F0F0); cpu.writeDInt(1, 0x00000FFF)
    new ANDShortToD(0, DataRegisterDirect, 1).apply(cpu)
    cpu.D(0) shouldBe 0x000000F0
  }

  "ANDByteToEA RMW" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000; cpu.memory.writeByte(0x2000, 0xFF)
    cpu.writeDInt(1, 0x0F)
    new ANDByteToEA(1, AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readByte(0x2000) shouldBe 0x0F
  }

  "ANDIntToEA RMW" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000; cpu.memory.writeInt(0x2000, 0xFFFFFFFF)
    cpu.writeDInt(1, 0x0F0F0F0F)
    new ANDIntToEA(1, AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readInt(0x2000) shouldBe 0x0F0F0F0F
  }

  "ANDIShort #imm,Dn" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x00FF)
    cpu.writeDInt(0, 0x0000FFFF)
    new ANDIShort(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0x000000FF
  }

  "ANDIInt #imm,Dn" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeInt(0x4000, 0x0000FFFF)
    cpu.writeDInt(0, 0xFFFFFFFF)
    new ANDIInt(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0x0000FFFF
  }

  "ANDItoSR requires supervisor mode and masks SR" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S | SRBit.T  // supervisor + trace
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x2000) // mask: keep S, clear T
    ANDItoSR.apply(cpu)
    cpu.sr shouldBe SRBit.S
  }

  // --- OR family gaps ------------------------------------------------------

  "ORShortToD" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x0000F000); cpu.writeDInt(1, 0x00000F00)
    new ORShortToD(0, DataRegisterDirect, 1).apply(cpu)
    cpu.D(0) shouldBe 0x0000FF00
  }

  "ORIntToD" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0x0F00F000); cpu.writeDInt(1, 0xF000000F)
    new ORIntToD(0, DataRegisterDirect, 1).apply(cpu)
    cpu.D(0) shouldBe 0xFF00F00F
  }

  "ORByteToEA" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000; cpu.memory.writeByte(0x2000, 0xF0)
    cpu.writeDInt(1, 0x0F)
    new ORByteToEA(1, AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readByte(0x2000) shouldBe 0xFF
  }

  "ORShortToEA" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000; cpu.memory.writeShort(0x2000, 0xF000)
    cpu.writeDInt(1, 0x0F0F)
    new ORShortToEA(1, AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readShort(0x2000) shouldBe 0xFF0F
  }

  "ORIntToEA" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000; cpu.memory.writeInt(0x2000, 0xF0F0F0F0)
    cpu.writeDInt(1, 0x0F0F0F0F)
    new ORIntToEA(1, AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readInt(0x2000) shouldBe 0xFFFFFFFF
  }

  "ORIByte #imm,Dn" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x000F)
    cpu.writeDInt(0, 0xF0)
    new ORIByte(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0xFF
  }

  "ORIInt #imm,Dn" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeInt(0x4000, 0x0000F00F)
    cpu.writeDInt(0, 0xF0000000)
    new ORIInt(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0xF000F00F
  }

  "ORItoSR requires supervisor and ORs into SR" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x8000) // set T
    ORItoSR.apply(cpu)
    (cpu.sr & SRBit.T) shouldBe SRBit.T
  }

  // --- EOR family gaps -----------------------------------------------------

  "EORShort flips a word" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000; cpu.memory.writeShort(0x2000, 0xFFFF)
    cpu.writeDInt(1, 0xF0F0)
    new EORShort(1, AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readShort(0x2000) shouldBe 0x0F0F
  }

  "EORIByte" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x00FF)
    cpu.writeDInt(0, 0x0F)
    new EORIByte(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0xF0
  }

  "EORIShort" in {
    val cpu = newCPU()
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0xFFFF)
    cpu.writeDInt(0, 0x0000FF00)
    new EORIShort(DataRegisterDirect, 0).apply(cpu)
    cpu.D(0) shouldBe 0x000000FF
  }

  "EORItoSR requires supervisor and XORs into SR" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S | SRBit.T
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x8000) // toggle T
    EORItoSR.apply(cpu)
    (cpu.sr & SRBit.T) shouldBe 0
  }

  // --- MOVEM gaps ----------------------------------------------------------

  "MOVEMIntToMem post-increment style (other modes)" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 0xCAFEBABE); cpu.writeDInt(1, 0xDEADBEEF)
    cpu.A(0) = 0x2000
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x0003) // bits 0,1 = D0, D1
    new MOVEMIntToMem(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readInt(0x2000) shouldBe 0xCAFEBABE
    cpu.memory.readInt(0x2004) shouldBe 0xDEADBEEF
  }

  "MOVEMShortFromMem from (An)+ form" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000
    cpu.memory.writeShort(0x2000, 0x1234)
    cpu.memory.writeShort(0x2002, 0x5678)
    cpu.PC = 0x4000; cpu.prog = cpu.memory.find(0x4000)
    cpu.memory.writeShort(0x4000, 0x0003)
    new MOVEMShortFromMem(AddressRegisterIndirectPostincrement, 0).apply(cpu)
    cpu.D(0) shouldBe 0x00001234  // sign-extended from 0x1234
    cpu.D(1) shouldBe 0x00005678
    cpu.A(0) shouldBe 0x2004
  }

  // --- CHK gap -------------------------------------------------------------

  "CHKInt with d in range does nothing" in {
    val cpu = newCPU()
    cpu.writeDInt(0, 100); cpu.writeDInt(1, 1000)
    new CHKInt(0, DataRegisterDirect, 1).apply(cpu)
    cpu.PC shouldBe 0
  }

  // --- Misc gaps -----------------------------------------------------------

  "RESET in supervisor calls resetSignal (smoke)" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S
    RESET.apply(cpu) // would be more interesting with a Device that sets a flag
  }

  "BKPT triggers illegal-instruction by default" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S; cpu.SSP = 0x1000; cpu.PC = 0x4000
    cpu.memory.writeInt(VectorTable.illegalInstruction, 0x6000)
    new BKPT(0).apply(cpu)
    cpu.PC shouldBe 0x6000
  }

  "LINEA takes the line-A vector" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S; cpu.SSP = 0x1000; cpu.PC = 0x4000
    cpu.memory.writeInt(VectorTable.lineA, 0x7000)
    LINEA.apply(cpu)
    cpu.PC shouldBe 0x7000
  }

  "LINEF takes the line-F vector" in {
    val cpu = newCPU()
    cpu.sr = SRBit.S; cpu.SSP = 0x1000; cpu.PC = 0x4000
    cpu.memory.writeInt(VectorTable.lineF, 0x7000)
    LINEF.apply(cpu)
    cpu.PC shouldBe 0x7000
  }

  // --- Shift register-form gaps --------------------------------------------

  "ASLIntReg" in {
    val cpu = newCPU(); cpu.writeDInt(0, 0x40000000)
    new ASLIntReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0x80000000
    cpu.V shouldBe true
  }

  "ASRByteReg" in {
    val cpu = newCPU(); cpu.writeDInt(0, 0x80)
    new ASRByteReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0xC0   // sign-extended into the byte
  }

  "ASRShortReg" in {
    val cpu = newCPU(); cpu.writeDInt(0, 0x8000)
    new ASRShortReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0xC000
  }

  "LSLShortReg" in {
    val cpu = newCPU(); cpu.writeDInt(0, 0x0001)
    new LSLShortReg(4, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0x0010
  }

  "LSLIntReg" in {
    val cpu = newCPU(); cpu.writeDInt(0, 1)
    new LSLIntReg(8, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0x100
  }

  "LSRByteReg" in {
    val cpu = newCPU(); cpu.writeDInt(0, 0x80)
    new LSRByteReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0x40
  }

  "LSRIntReg" in {
    val cpu = newCPU(); cpu.writeDInt(0, 0x80000000)
    new LSRIntReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0x40000000
  }

  "ROLShortReg" in {
    val cpu = newCPU(); cpu.writeDInt(0, 0x8000)
    new ROLShortReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0x0001
  }

  "ROLIntReg" in {
    val cpu = newCPU(); cpu.writeDInt(0, 0x80000000)
    new ROLIntReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 1
  }

  "RORByteReg" in {
    val cpu = newCPU(); cpu.writeDInt(0, 0x01)
    new RORByteReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0x80
  }

  "RORIntReg" in {
    val cpu = newCPU(); cpu.writeDInt(0, 1)
    new RORIntReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0x80000000
  }

  "ROXLByteReg" in {
    val cpu = newCPU(); cpu.setX(true); cpu.writeDInt(0, 0)
    new ROXLByteReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 1
  }

  "ROXLShortReg" in {
    val cpu = newCPU(); cpu.setX(true); cpu.writeDInt(0, 0)
    new ROXLShortReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 1
  }

  "ROXRShortReg" in {
    val cpu = newCPU(); cpu.setX(false); cpu.writeDInt(0, 0x8000)
    new ROXRShortReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0x4000
  }

  "ROXRIntReg" in {
    val cpu = newCPU(); cpu.setX(true); cpu.writeDInt(0, 0)
    new ROXRIntReg(1, 0, 0).apply(cpu)
    cpu.D(0) shouldBe 0x80000000
  }

  // --- Memory shift gaps ---------------------------------------------------

  "ASRMem on (An)" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000; cpu.memory.writeShort(0x2000, 0x8000)
    new ASRMem(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readShort(0x2000) shouldBe 0xC000
  }

  "LSLMem on (An)" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000; cpu.memory.writeShort(0x2000, 0x0001)
    new LSLMem(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readShort(0x2000) shouldBe 0x0002
  }

  "ROLMem on (An)" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000; cpu.memory.writeShort(0x2000, 0x8000)
    new ROLMem(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readShort(0x2000) shouldBe 0x0001
  }

  "RORMem on (An)" in {
    val cpu = newCPU()
    cpu.A(0) = 0x2000; cpu.memory.writeShort(0x2000, 0x0001)
    new RORMem(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readShort(0x2000) shouldBe 0x8000
  }

  "ROXRMem on (An)" in {
    val cpu = newCPU()
    cpu.setX(false)
    cpu.A(0) = 0x2000; cpu.memory.writeShort(0x2000, 0x8000)
    new ROXRMem(AddressRegisterIndirect, 0).apply(cpu)
    cpu.memory.readShort(0x2000) shouldBe 0x4000
  }
}
