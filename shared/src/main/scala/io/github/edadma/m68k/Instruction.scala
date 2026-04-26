package io.github.edadma.m68k

import scala.collection.mutable.ListBuffer

import Addressing.*

/** Decoded m68k instruction.
  *
  * Built once per encoding at class-load time and indexed into a 64K-entry array by raw opcode (`apply` is a single
  * virtual call after one array index — no per-cycle decode work). Instructions never carry a `Size` parameter
  * matched on at runtime: width-parameterised opcodes (ADD, MOVE, ASL, ...) split into per-width subclasses, so the
  * width is baked into the call target at decode time. The hot path therefore avoids Scala's `match` discriminator
  * boxing entirely.
  */
abstract class Instruction extends (CPU => Unit) {

  /** Mnemonic with `.B/.W/.L` suffix and width-aware spacing. */
  protected def mnem(s: String): String = s"$s${" " * (8 - s.length)} "

  protected def mnem(s: String, suffix: Char): String = s"$s.$suffix${" " * (6 - s.length)} "

  def disassemble(cpu: CPU): String
}

/** ILLEGAL — m68k traps to the illegal-instruction vector unless the host overrides `cpu.illegal`. */
object ILLEGAL extends Instruction {
  def apply(cpu: CPU): Unit =
    if !cpu.illegal then cpu.exception(-1, VectorTable.illegalInstruction)
  def disassemble(cpu: CPU): String = "ILLEGAL"
}

object NOP extends Instruction {
  def apply(cpu: CPU): Unit         = ()
  def disassemble(cpu: CPU): String = "NOP"
}

// ============================================================================
// ADD family — Dn ⇄ ea
// ============================================================================

/** ADD where the destination is the data register (`dir == 0`). The source EA can be any addressing mode. */
final class ADDByteToD(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readByte(mode, reg)
    val d = cpu.readDByte(dreg)
    cpu.writeDByte(dreg, cpu.addByte(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("ADD", 'B')
}

final class ADDShortToD(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readShort(mode, reg)
    val d = cpu.readDShort(dreg)
    cpu.writeDShort(dreg, cpu.addShort(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("ADD", 'W')
}

final class ADDIntToD(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readInt(mode, reg)
    val d = cpu.readDInt(dreg)
    cpu.writeDInt(dreg, cpu.addInt(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("ADD", 'L')
}

/** ADD where the destination is the EA (memory mode), source is Dn. Inline RMW. */
final class ADDByteToEA(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readDByte(dreg)
    cpu.readWriteByte(mode, reg)(d => cpu.addByte(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("ADD", 'B')
}

final class ADDShortToEA(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readDShort(dreg)
    cpu.readWriteShort(mode, reg)(d => cpu.addShort(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("ADD", 'W')
}

final class ADDIntToEA(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readDInt(dreg)
    cpu.readWriteInt(mode, reg)(d => cpu.addInt(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("ADD", 'L')
}

/** ADDA — destination An, no flag effects (the m68k discards CCR for An ops).
  *
  * Source is sign-extended to 32 bits regardless of the size suffix; the result writes the full register.
  */
final class ADDAShort(areg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readShort(mode, reg)               // already sign-extended by readShort
    cpu.writeA(areg, cpu.readA(areg) + s)
  }
  def disassemble(cpu: CPU): String = mnem("ADDA", 'W')
}

final class ADDAInt(areg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readInt(mode, reg)
    cpu.writeA(areg, cpu.readA(areg) + s)
  }
  def disassemble(cpu: CPU): String = mnem("ADDA", 'L')
}

/** ADDI — immediate source, EA destination (which can be Dn or memory). */
final class ADDIByte(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = cpu.fetchByte()
    cpu.readWriteByte(mode, reg)(d => cpu.addByte(imm, d))
  }
  def disassemble(cpu: CPU): String = mnem("ADDI", 'B')
}

final class ADDIShort(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = (cpu.fetchShort() << 16) >> 16
    cpu.readWriteShort(mode, reg)(d => cpu.addShort(imm, d))
  }
  def disassemble(cpu: CPU): String = mnem("ADDI", 'W')
}

final class ADDIInt(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = cpu.fetchInt()
    cpu.readWriteInt(mode, reg)(d => cpu.addInt(imm, d))
  }
  def disassemble(cpu: CPU): String = mnem("ADDI", 'L')
}

/** ADDQ — quick add of an immediate 1..8 to EA. When the destination is An, ADDQ doesn't update flags and uses
  * full 32-bit math (the m68k treats ADDQ.W #n,An as if it were ADDQ.L). */
final class ADDQByte(data: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if mode == AddressRegisterDirect then cpu.writeA(reg, cpu.readA(reg) + data)
    else cpu.readWriteByte(mode, reg)(d => cpu.addByte(data, d))
  def disassemble(cpu: CPU): String = mnem("ADDQ", 'B')
}

final class ADDQShort(data: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if mode == AddressRegisterDirect then cpu.writeA(reg, cpu.readA(reg) + data)
    else cpu.readWriteShort(mode, reg)(d => cpu.addShort(data, d))
  def disassemble(cpu: CPU): String = mnem("ADDQ", 'W')
}

final class ADDQInt(data: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if mode == AddressRegisterDirect then cpu.writeA(reg, cpu.readA(reg) + data)
    else cpu.readWriteInt(mode, reg)(d => cpu.addInt(data, d))
  def disassemble(cpu: CPU): String = mnem("ADDQ", 'L')
}

/** ADDX — extended add (uses X as carry-in, sticky Z). Two operand modes: Dn,Dn or -(Ay),-(Ax). */
final class ADDXByte(regx: Int, rm: Int, regy: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if rm == 0 then cpu.writeDByte(regx, cpu.addxByte(cpu.readDByte(regy), cpu.readDByte(regx)))
    else {
      val sAddr = cpu.predecrementA(regy, 1); val s = cpu.memory.readByte(sAddr)
      val dAddr = cpu.predecrementA(regx, 1); val d = cpu.memory.readByte(dAddr)
      cpu.memory.writeByte(dAddr, cpu.addxByte(s, d))
    }
  def disassemble(cpu: CPU): String = mnem("ADDX", 'B')
}

final class ADDXShort(regx: Int, rm: Int, regy: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if rm == 0 then cpu.writeDShort(regx, cpu.addxShort(cpu.readDShort(regy), cpu.readDShort(regx)))
    else {
      val sAddr = cpu.predecrementA(regy, 2); val s = cpu.memory.readShort(sAddr)
      val dAddr = cpu.predecrementA(regx, 2); val d = cpu.memory.readShort(dAddr)
      cpu.memory.writeShort(dAddr, cpu.addxShort(s, d))
    }
  def disassemble(cpu: CPU): String = mnem("ADDX", 'W')
}

final class ADDXInt(regx: Int, rm: Int, regy: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if rm == 0 then cpu.writeDInt(regx, cpu.addxInt(cpu.readDInt(regy), cpu.readDInt(regx)))
    else {
      val sAddr = cpu.predecrementA(regy, 4); val s = cpu.memory.readInt(sAddr)
      val dAddr = cpu.predecrementA(regx, 4); val d = cpu.memory.readInt(dAddr)
      cpu.memory.writeInt(dAddr, cpu.addxInt(s, d))
    }
  def disassemble(cpu: CPU): String = mnem("ADDX", 'L')
}

// ============================================================================
// SUB family — mirrors ADD, but the EA-destination form computes ea − Dn
// (`subByte(s = Dn, d = ea)` returns `ea − Dn`, then writes back to ea).
// ============================================================================

final class SUBByteToD(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readByte(mode, reg)
    val d = cpu.readDByte(dreg)
    cpu.writeDByte(dreg, cpu.subByte(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("SUB", 'B')
}

final class SUBShortToD(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readShort(mode, reg)
    val d = cpu.readDShort(dreg)
    cpu.writeDShort(dreg, cpu.subShort(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("SUB", 'W')
}

final class SUBIntToD(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readInt(mode, reg)
    val d = cpu.readDInt(dreg)
    cpu.writeDInt(dreg, cpu.subInt(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("SUB", 'L')
}

final class SUBByteToEA(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readDByte(dreg)
    cpu.readWriteByte(mode, reg)(d => cpu.subByte(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("SUB", 'B')
}

final class SUBShortToEA(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readDShort(dreg)
    cpu.readWriteShort(mode, reg)(d => cpu.subShort(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("SUB", 'W')
}

final class SUBIntToEA(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readDInt(dreg)
    cpu.readWriteInt(mode, reg)(d => cpu.subInt(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("SUB", 'L')
}

/** SUBA — destination An; no flag effects, source sign-extended to 32 bits. */
final class SUBAShort(areg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readShort(mode, reg)
    cpu.writeA(areg, cpu.readA(areg) - s)
  }
  def disassemble(cpu: CPU): String = mnem("SUBA", 'W')
}

final class SUBAInt(areg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readInt(mode, reg)
    cpu.writeA(areg, cpu.readA(areg) - s)
  }
  def disassemble(cpu: CPU): String = mnem("SUBA", 'L')
}

final class SUBIByte(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = cpu.fetchByte()
    cpu.readWriteByte(mode, reg)(d => cpu.subByte(imm, d))
  }
  def disassemble(cpu: CPU): String = mnem("SUBI", 'B')
}

final class SUBIShort(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = (cpu.fetchShort() << 16) >> 16
    cpu.readWriteShort(mode, reg)(d => cpu.subShort(imm, d))
  }
  def disassemble(cpu: CPU): String = mnem("SUBI", 'W')
}

final class SUBIInt(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = cpu.fetchInt()
    cpu.readWriteInt(mode, reg)(d => cpu.subInt(imm, d))
  }
  def disassemble(cpu: CPU): String = mnem("SUBI", 'L')
}

final class SUBQByte(data: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if mode == AddressRegisterDirect then cpu.writeA(reg, cpu.readA(reg) - data)
    else cpu.readWriteByte(mode, reg)(d => cpu.subByte(data, d))
  def disassemble(cpu: CPU): String = mnem("SUBQ", 'B')
}

final class SUBQShort(data: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if mode == AddressRegisterDirect then cpu.writeA(reg, cpu.readA(reg) - data)
    else cpu.readWriteShort(mode, reg)(d => cpu.subShort(data, d))
  def disassemble(cpu: CPU): String = mnem("SUBQ", 'W')
}

final class SUBQInt(data: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if mode == AddressRegisterDirect then cpu.writeA(reg, cpu.readA(reg) - data)
    else cpu.readWriteInt(mode, reg)(d => cpu.subInt(data, d))
  def disassemble(cpu: CPU): String = mnem("SUBQ", 'L')
}

/** SUBX — `regx` is the source (Ry), `regy` is the destination (Rx). Naming follows the original dispatch
  * table; semantics are `regy := regy − regx − X` with sticky Z. */
final class SUBXByte(regx: Int, rm: Int, regy: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if rm == 0 then cpu.writeDByte(regy, cpu.subxByte(cpu.readDByte(regx), cpu.readDByte(regy)))
    else {
      val sAddr = cpu.predecrementA(regx, 1); val s = cpu.memory.readByte(sAddr)
      val dAddr = cpu.predecrementA(regy, 1); val d = cpu.memory.readByte(dAddr)
      cpu.memory.writeByte(dAddr, cpu.subxByte(s, d))
    }
  def disassemble(cpu: CPU): String = mnem("SUBX", 'B')
}

final class SUBXShort(regx: Int, rm: Int, regy: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if rm == 0 then cpu.writeDShort(regy, cpu.subxShort(cpu.readDShort(regx), cpu.readDShort(regy)))
    else {
      val sAddr = cpu.predecrementA(regx, 2); val s = cpu.memory.readShort(sAddr)
      val dAddr = cpu.predecrementA(regy, 2); val d = cpu.memory.readShort(dAddr)
      cpu.memory.writeShort(dAddr, cpu.subxShort(s, d))
    }
  def disassemble(cpu: CPU): String = mnem("SUBX", 'W')
}

final class SUBXInt(regx: Int, rm: Int, regy: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if rm == 0 then cpu.writeDInt(regy, cpu.subxInt(cpu.readDInt(regx), cpu.readDInt(regy)))
    else {
      val sAddr = cpu.predecrementA(regx, 4); val s = cpu.memory.readInt(sAddr)
      val dAddr = cpu.predecrementA(regy, 4); val d = cpu.memory.readInt(dAddr)
      cpu.memory.writeInt(dAddr, cpu.subxInt(s, d))
    }
  def disassemble(cpu: CPU): String = mnem("SUBX", 'L')
}

// ============================================================================
// CMP family — same as SUB but discards the result and leaves X alone
// ============================================================================

final class CMPByte(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.cmpByte(cpu.readByte(mode, reg), cpu.readDByte(dreg))
  def disassemble(cpu: CPU): String = mnem("CMP", 'B')
}

final class CMPShort(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.cmpShort(cpu.readShort(mode, reg), cpu.readDShort(dreg))
  def disassemble(cpu: CPU): String = mnem("CMP", 'W')
}

final class CMPInt(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.cmpInt(cpu.readInt(mode, reg), cpu.readDInt(dreg))
  def disassemble(cpu: CPU): String = mnem("CMP", 'L')
}

/** CMPA — compare against An. Source sign-extended to 32 bits. */
final class CMPAShort(areg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.cmpInt(cpu.readShort(mode, reg), cpu.readA(areg))
  def disassemble(cpu: CPU): String = mnem("CMPA", 'W')
}

final class CMPAInt(areg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.cmpInt(cpu.readInt(mode, reg), cpu.readA(areg))
  def disassemble(cpu: CPU): String = mnem("CMPA", 'L')
}

final class CMPIByte(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = cpu.fetchByte()
    cpu.cmpByte(imm, cpu.readByte(mode, reg))
  }
  def disassemble(cpu: CPU): String = mnem("CMPI", 'B')
}

final class CMPIShort(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = (cpu.fetchShort() << 16) >> 16
    cpu.cmpShort(imm, cpu.readShort(mode, reg))
  }
  def disassemble(cpu: CPU): String = mnem("CMPI", 'W')
}

final class CMPIInt(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = cpu.fetchInt()
    cpu.cmpInt(imm, cpu.readInt(mode, reg))
  }
  def disassemble(cpu: CPU): String = mnem("CMPI", 'L')
}

/** CMPM (Ay)+,(Ax)+ — both operands consumed via post-increment. */
final class CMPMByte(rx: Int, ry: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val sAddr = cpu.postincrementA(ry, 1); val s = (cpu.memory.readByte(sAddr) << 24) >> 24
    val dAddr = cpu.postincrementA(rx, 1); val d = (cpu.memory.readByte(dAddr) << 24) >> 24
    cpu.cmpByte(s, d)
  }
  def disassemble(cpu: CPU): String = mnem("CMPM", 'B')
}

final class CMPMShort(rx: Int, ry: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val sAddr = cpu.postincrementA(ry, 2); val s = (cpu.memory.readShort(sAddr) << 16) >> 16
    val dAddr = cpu.postincrementA(rx, 2); val d = (cpu.memory.readShort(dAddr) << 16) >> 16
    cpu.cmpShort(s, d)
  }
  def disassemble(cpu: CPU): String = mnem("CMPM", 'W')
}

final class CMPMInt(rx: Int, ry: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val sAddr = cpu.postincrementA(ry, 4); val s = cpu.memory.readInt(sAddr)
    val dAddr = cpu.postincrementA(rx, 4); val d = cpu.memory.readInt(dAddr)
    cpu.cmpInt(s, d)
  }
  def disassemble(cpu: CPU): String = mnem("CMPM", 'L')
}

// ============================================================================
// NEG / NEGX / NOT / CLR / TST / EXT — single-operand width-specialised
// ============================================================================

final class NEGByte(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteByte(mode, reg)(d => cpu.negByte(d))
  def disassemble(cpu: CPU): String = mnem("NEG", 'B')
}
final class NEGShort(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteShort(mode, reg)(d => cpu.negShort(d))
  def disassemble(cpu: CPU): String = mnem("NEG", 'W')
}
final class NEGInt(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteInt(mode, reg)(d => cpu.negInt(d))
  def disassemble(cpu: CPU): String = mnem("NEG", 'L')
}

final class NEGXByte(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteByte(mode, reg)(d => cpu.negxByte(d))
  def disassemble(cpu: CPU): String = mnem("NEGX", 'B')
}
final class NEGXShort(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteShort(mode, reg)(d => cpu.negxShort(d))
  def disassemble(cpu: CPU): String = mnem("NEGX", 'W')
}
final class NEGXInt(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteInt(mode, reg)(d => cpu.negxInt(d))
  def disassemble(cpu: CPU): String = mnem("NEGX", 'L')
}

final class NOTByte(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteByte(mode, reg)(d => cpu.notByte(d))
  def disassemble(cpu: CPU): String = mnem("NOT", 'B')
}
final class NOTShort(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteShort(mode, reg)(d => cpu.notShort(d))
  def disassemble(cpu: CPU): String = mnem("NOT", 'W')
}
final class NOTInt(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteInt(mode, reg)(d => cpu.notInt(d))
  def disassemble(cpu: CPU): String = mnem("NOT", 'L')
}

/** CLR — write zero to EA and set CCR to "Z, all others clear" (X preserved per m68k spec). */
final class CLRByte(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    cpu.writeByte(mode, reg, 0)
    cpu.ccr = (cpu.ccr & CCR.X) | CCR.Z
  }
  def disassemble(cpu: CPU): String = mnem("CLR", 'B')
}
final class CLRShort(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    cpu.writeShort(mode, reg, 0)
    cpu.ccr = (cpu.ccr & CCR.X) | CCR.Z
  }
  def disassemble(cpu: CPU): String = mnem("CLR", 'W')
}
final class CLRInt(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    cpu.writeInt(mode, reg, 0)
    cpu.ccr = (cpu.ccr & CCR.X) | CCR.Z
  }
  def disassemble(cpu: CPU): String = mnem("CLR", 'L')
}

final class TSTByte(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.tstByte(cpu.readByte(mode, reg))
  def disassemble(cpu: CPU): String = mnem("TST", 'B')
}
final class TSTShort(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.tstShort(cpu.readShort(mode, reg))
  def disassemble(cpu: CPU): String = mnem("TST", 'W')
}
final class TSTInt(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.tstInt(cpu.readInt(mode, reg))
  def disassemble(cpu: CPU): String = mnem("TST", 'L')
}

/** EXT.W — sign-extend low byte of Dn into low word. EXT.L — sign-extend low word into the full register. */
final class EXTShort(reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val v = cpu.readDByte(reg) & 0xFFFF
    cpu.writeDShort(reg, v)
    cpu.tstShort(v)
  }
  def disassemble(cpu: CPU): String = mnem("EXT", 'W')
}
final class EXTInt(reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val v = cpu.readDShort(reg)
    cpu.writeDInt(reg, v)
    cpu.tstInt(v)
  }
  def disassemble(cpu: CPU): String = mnem("EXT", 'L')
}

/** SWAP — exchange high and low 16-bit halves of Dn; logic-flag CCR update on the new value. */
final class SWAP(reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val v   = cpu.D(reg)
    val res = (v << 16) | (v >>> 16)
    cpu.D(reg) = res
    cpu.ccr = (cpu.ccr & CCR.X) | (if res == 0 then CCR.Z else 0) | (if res < 0 then CCR.N else 0)
  }
  def disassemble(cpu: CPU): String = mnem("SWAP") + s"D$reg"
}

// ============================================================================
// Logic — AND / OR / EOR + immediate variants. EORI/ANDI/ORI to CCR/SR are
// handled via direct CCR/SR mask, which fixes the bit-by-bit duplication bug
// in the original code.
// ============================================================================

final class ANDByteToD(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    cpu.writeDByte(dreg, cpu.andByte(cpu.readByte(mode, reg), cpu.readDByte(dreg)))
  def disassemble(cpu: CPU): String = mnem("AND", 'B')
}
final class ANDShortToD(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    cpu.writeDShort(dreg, cpu.andShort(cpu.readShort(mode, reg), cpu.readDShort(dreg)))
  def disassemble(cpu: CPU): String = mnem("AND", 'W')
}
final class ANDIntToD(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    cpu.writeDInt(dreg, cpu.andInt(cpu.readInt(mode, reg), cpu.readDInt(dreg)))
  def disassemble(cpu: CPU): String = mnem("AND", 'L')
}
final class ANDByteToEA(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readDByte(dreg)
    cpu.readWriteByte(mode, reg)(d => cpu.andByte(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("AND", 'B')
}
final class ANDShortToEA(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readDShort(dreg)
    cpu.readWriteShort(mode, reg)(d => cpu.andShort(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("AND", 'W')
}
final class ANDIntToEA(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readDInt(dreg)
    cpu.readWriteInt(mode, reg)(d => cpu.andInt(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("AND", 'L')
}

final class ORByteToD(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    cpu.writeDByte(dreg, cpu.orByte(cpu.readByte(mode, reg), cpu.readDByte(dreg)))
  def disassemble(cpu: CPU): String = mnem("OR", 'B')
}
final class ORShortToD(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    cpu.writeDShort(dreg, cpu.orShort(cpu.readShort(mode, reg), cpu.readDShort(dreg)))
  def disassemble(cpu: CPU): String = mnem("OR", 'W')
}
final class ORIntToD(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    cpu.writeDInt(dreg, cpu.orInt(cpu.readInt(mode, reg), cpu.readDInt(dreg)))
  def disassemble(cpu: CPU): String = mnem("OR", 'L')
}
final class ORByteToEA(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readDByte(dreg)
    cpu.readWriteByte(mode, reg)(d => cpu.orByte(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("OR", 'B')
}
final class ORShortToEA(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readDShort(dreg)
    cpu.readWriteShort(mode, reg)(d => cpu.orShort(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("OR", 'W')
}
final class ORIntToEA(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readDInt(dreg)
    cpu.readWriteInt(mode, reg)(d => cpu.orInt(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("OR", 'L')
}

/** EOR — destination is always the EA (the spec doesn't have an EOR-to-Dn variant). */
final class EORByte(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readDByte(dreg)
    cpu.readWriteByte(mode, reg)(d => cpu.eorByte(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("EOR", 'B')
}
final class EORShort(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readDShort(dreg)
    cpu.readWriteShort(mode, reg)(d => cpu.eorShort(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("EOR", 'W')
}
final class EORInt(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val s = cpu.readDInt(dreg)
    cpu.readWriteInt(mode, reg)(d => cpu.eorInt(s, d))
  }
  def disassemble(cpu: CPU): String = mnem("EOR", 'L')
}

final class ANDIByte(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = cpu.fetchByte()
    cpu.readWriteByte(mode, reg)(d => cpu.andByte(imm, d))
  }
  def disassemble(cpu: CPU): String = mnem("ANDI", 'B')
}
final class ANDIShort(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = (cpu.fetchShort() << 16) >> 16
    cpu.readWriteShort(mode, reg)(d => cpu.andShort(imm, d))
  }
  def disassemble(cpu: CPU): String = mnem("ANDI", 'W')
}
final class ANDIInt(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = cpu.fetchInt()
    cpu.readWriteInt(mode, reg)(d => cpu.andInt(imm, d))
  }
  def disassemble(cpu: CPU): String = mnem("ANDI", 'L')
}

final class ORIByte(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = cpu.fetchByte()
    cpu.readWriteByte(mode, reg)(d => cpu.orByte(imm, d))
  }
  def disassemble(cpu: CPU): String = mnem("ORI", 'B')
}
final class ORIShort(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = (cpu.fetchShort() << 16) >> 16
    cpu.readWriteShort(mode, reg)(d => cpu.orShort(imm, d))
  }
  def disassemble(cpu: CPU): String = mnem("ORI", 'W')
}
final class ORIInt(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = cpu.fetchInt()
    cpu.readWriteInt(mode, reg)(d => cpu.orInt(imm, d))
  }
  def disassemble(cpu: CPU): String = mnem("ORI", 'L')
}

final class EORIByte(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = cpu.fetchByte()
    cpu.readWriteByte(mode, reg)(d => cpu.eorByte(imm, d))
  }
  def disassemble(cpu: CPU): String = mnem("EORI", 'B')
}
final class EORIShort(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = (cpu.fetchShort() << 16) >> 16
    cpu.readWriteShort(mode, reg)(d => cpu.eorShort(imm, d))
  }
  def disassemble(cpu: CPU): String = mnem("EORI", 'W')
}
final class EORIInt(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = cpu.fetchInt()
    cpu.readWriteInt(mode, reg)(d => cpu.eorInt(imm, d))
  }
  def disassemble(cpu: CPU): String = mnem("EORI", 'L')
}

/** ANDI #imm,CCR — clears CCR bits where the immediate has 0. The original code had a duplicate-V/missing-C
  * bug (X N Z V V instead of X N Z V C); we fix it by treating the immediate as a single 5-bit mask. */
object ANDItoCCR extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = cpu.fetchShort() & 0xFF // immediate is in low byte of an extension word
    cpu.ccr = cpu.ccr & (imm & CCR.Mask)
  }
  def disassemble(cpu: CPU): String = mnem("ANDI") + "#imm, CCR"
}
object ORItoCCR extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = cpu.fetchShort() & 0xFF
    cpu.ccr = cpu.ccr | (imm & CCR.Mask)
  }
  def disassemble(cpu: CPU): String = mnem("ORI") + "#imm, CCR"
}
object EORItoCCR extends Instruction {
  def apply(cpu: CPU): Unit = {
    val imm = cpu.fetchShort() & 0xFF
    cpu.ccr = cpu.ccr ^ (imm & CCR.Mask)
  }
  def disassemble(cpu: CPU): String = mnem("EORI") + "#imm, CCR"
}

object ANDItoSR extends Instruction {
  def apply(cpu: CPU): Unit =
    if cpu.supervisor then cpu.toSR(cpu.fromSR & cpu.fetchShort())
  def disassemble(cpu: CPU): String = mnem("ANDI") + "#imm, SR"
}
object ORItoSR extends Instruction {
  def apply(cpu: CPU): Unit =
    if cpu.supervisor then cpu.toSR(cpu.fromSR | cpu.fetchShort())
  def disassemble(cpu: CPU): String = mnem("ORI") + "#imm, SR"
}
object EORItoSR extends Instruction {
  def apply(cpu: CPU): Unit =
    if cpu.supervisor then cpu.toSR(cpu.fromSR ^ cpu.fetchShort())
  def disassemble(cpu: CPU): String = mnem("EORI") + "#imm, SR"
}

// ============================================================================
// MOVE family
// ============================================================================

/** MOVE.B/W/L — read source, run logic-flag update, write destination. */
final class MOVEByte(dreg: Int, dmode: Int, smode: Int, sreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val v = cpu.readByte(smode, sreg)
    cpu.tstByte(v)
    cpu.writeByte(dmode, dreg, v)
  }
  def disassemble(cpu: CPU): String = mnem("MOVE", 'B')
}
final class MOVEShort(dreg: Int, dmode: Int, smode: Int, sreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val v = cpu.readShort(smode, sreg)
    cpu.tstShort(v)
    cpu.writeShort(dmode, dreg, v)
  }
  def disassemble(cpu: CPU): String = mnem("MOVE", 'W')
}
final class MOVEInt(dreg: Int, dmode: Int, smode: Int, sreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val v = cpu.readInt(smode, sreg)
    cpu.tstInt(v)
    cpu.writeInt(dmode, dreg, v)
  }
  def disassemble(cpu: CPU): String = mnem("MOVE", 'L')
}

/** MOVEA.W sign-extends to 32 bits; MOVEA.L copies the full register. Neither touches CCR. */
final class MOVEAShort(areg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.writeA(areg, cpu.readShort(mode, reg))
  def disassemble(cpu: CPU): String = mnem("MOVEA", 'W')
}
final class MOVEAInt(areg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.writeA(areg, cpu.readInt(mode, reg))
  def disassemble(cpu: CPU): String = mnem("MOVEA", 'L')
}

/** MOVEQ — sign-extended 8-bit immediate to Dn (full 32-bit write). */
final class MOVEQ(reg: Int, data: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val v = (data << 24) >> 24 // already an Int; dispatch table passes the byte sign-extended already
    val sx = data // assume caller provided sign-extended Int
    cpu.D(reg) = sx
    cpu.tstInt(sx)
  }
  def disassemble(cpu: CPU): String = mnem("MOVEQ") + s"#$data, D$reg"
}

final class MOVEUSP(dir: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if cpu.supervisor then
      if dir == 0 then cpu.USP = cpu.readA(reg) // An → USP
      else cpu.writeA(reg, cpu.USP)             // USP → An
  def disassemble(cpu: CPU): String = mnem("MOVE") + (if dir == 0 then s"A$reg, USP" else s"USP, A$reg")
}

final class MOVEfromSR(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.writeShort(mode, reg, cpu.fromSR)
  def disassemble(cpu: CPU): String = mnem("MOVE") + "SR, ea"
}
final class MOVEtoCCR(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.toCCR(cpu.readByte(mode, reg))
  def disassemble(cpu: CPU): String = mnem("MOVE") + "ea, CCR"
}
final class MOVEtoSR(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = if cpu.supervisor then cpu.toSR(cpu.readShort(mode, reg))
  def disassemble(cpu: CPU): String = mnem("MOVE") + "ea, SR"
}

/** MOVEM — multi-register move. The list word selects which of D0..D7,A0..A7 participate. The bit ordering of
  * the list word depends on the EA mode: predec uses A7..D0 (reversed), all other modes use D0..A7. */
final class MOVEMShortToMem(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val list = cpu.fetchShort() & 0xFFFF
    if mode == AddressRegisterIndirectPredecrement then {
      // bit 0 of list = A7, bit 15 = D0
      var i = 0
      while i < 16 do {
        if (list & (1 << i)) != 0 then {
          val v = if i < 8 then cpu.readA(7 - i) else cpu.D(7 - (i - 8))
          val addr = cpu.predecrementA(reg, 2)
          cpu.memory.writeShort(addr, v)
        }
        i += 1
      }
    } else {
      var addr = cpu.address(mode, reg, 2)
      var i = 0
      while i < 16 do {
        if (list & (1 << i)) != 0 then {
          val v = if i < 8 then cpu.D(i) else cpu.readA(i - 8)
          cpu.memory.writeShort(addr, v)
          addr += 2
        }
        i += 1
      }
    }
  }
  def disassemble(cpu: CPU): String = mnem("MOVEM", 'W')
}

final class MOVEMIntToMem(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val list = cpu.fetchShort() & 0xFFFF
    if mode == AddressRegisterIndirectPredecrement then {
      var i = 0
      while i < 16 do {
        if (list & (1 << i)) != 0 then {
          val v = if i < 8 then cpu.readA(7 - i) else cpu.D(7 - (i - 8))
          val addr = cpu.predecrementA(reg, 4)
          cpu.memory.writeInt(addr, v)
        }
        i += 1
      }
    } else {
      var addr = cpu.address(mode, reg, 4)
      var i = 0
      while i < 16 do {
        if (list & (1 << i)) != 0 then {
          val v = if i < 8 then cpu.D(i) else cpu.readA(i - 8)
          cpu.memory.writeInt(addr, v)
          addr += 4
        }
        i += 1
      }
    }
  }
  def disassemble(cpu: CPU): String = mnem("MOVEM", 'L')
}

/** MOVEM mem → registers (dir=1). Sign-extended for the .W variant. */
final class MOVEMShortFromMem(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val list = cpu.fetchShort() & 0xFFFF
    if mode == AddressRegisterIndirectPostincrement then {
      var i = 0
      while i < 16 do {
        if (list & (1 << i)) != 0 then {
          val addr = cpu.postincrementA(reg, 2)
          val v    = (cpu.memory.readShort(addr) << 16) >> 16
          if i < 8 then cpu.D(i) = v else cpu.writeA(i - 8, v)
        }
        i += 1
      }
    } else {
      var addr = cpu.address(mode, reg, 2)
      var i = 0
      while i < 16 do {
        if (list & (1 << i)) != 0 then {
          val v = (cpu.memory.readShort(addr) << 16) >> 16
          if i < 8 then cpu.D(i) = v else cpu.writeA(i - 8, v)
          addr += 2
        }
        i += 1
      }
    }
  }
  def disassemble(cpu: CPU): String = mnem("MOVEM", 'W')
}

final class MOVEMIntFromMem(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val list = cpu.fetchShort() & 0xFFFF
    if mode == AddressRegisterIndirectPostincrement then {
      var i = 0
      while i < 16 do {
        if (list & (1 << i)) != 0 then {
          val addr = cpu.postincrementA(reg, 4)
          val v    = cpu.memory.readInt(addr)
          if i < 8 then cpu.D(i) = v else cpu.writeA(i - 8, v)
        }
        i += 1
      }
    } else {
      var addr = cpu.address(mode, reg, 4)
      var i = 0
      while i < 16 do {
        if (list & (1 << i)) != 0 then {
          val v = cpu.memory.readInt(addr)
          if i < 8 then cpu.D(i) = v else cpu.writeA(i - 8, v)
          addr += 4
        }
        i += 1
      }
    }
  }
  def disassemble(cpu: CPU): String = mnem("MOVEM", 'L')
}

// ============================================================================
// Branches and subroutine control
// ============================================================================

/** Bcc — 8-bit signed displacement embedded in the opcode; if 0 the displacement is the next 16-bit word; if -1
  * the displacement is the next 32-bit word (m68020 only, accepted here). */
final class Bcc(cond: Int, disp: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val pcAtDisp = cpu.PC
    val target = pcAtDisp + (disp match {
      case 0  => (cpu.fetchShort() << 16) >> 16
      case -1 => cpu.fetchInt()
      case _  => disp // already sign-extended
    })
    if cpu.testcc(cond) then cpu.jumpTo(target)
  }
  def disassemble(cpu: CPU): String = mnem(s"B${Conditional(cond)}") + s"disp=$disp"
}

final class BSR(disp: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val pcAtDisp = cpu.PC
    val target = pcAtDisp + (disp match {
      case 0  => (cpu.fetchShort() << 16) >> 16
      case -1 => cpu.fetchInt()
      case _  => disp
    })
    cpu.pushInt(cpu.PC)
    cpu.jumpTo(target)
  }
  def disassemble(cpu: CPU): String = mnem("BSR") + s"disp=$disp"
}

/** DBcc Dn,disp — if `cond` true, fall through; else decrement Dn.W; if it became -1, fall through; else branch. */
final class DBcc(cond: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    if cpu.testcc(cond) then cpu.PC += 2
    else {
      val res = (cpu.readDShort(reg) - 1) & 0xFFFF
      cpu.writeDShort(reg, res)
      val sx = (res << 16) >> 16
      if sx == -1 then cpu.PC += 2
      else {
        val pc = cpu.PC
        val disp = (cpu.fetchShort() << 16) >> 16
        cpu.jumpTo(pc + disp)
      }
    }
  }
  def disassemble(cpu: CPU): String = mnem(s"DB${Conditional(cond)}") + s"D$reg, disp"
}

/** Scc — write 0xFF to byte EA if condition true, else 0x00. */
final class Scc(cond: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.writeByte(mode, reg, if cpu.testcc(cond) then 0xFF else 0)
  def disassemble(cpu: CPU): String = mnem(s"S${Conditional(cond)}") + "ea"
}

final class JMP(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.jumpTo(cpu.address(mode, reg, 4))
  def disassemble(cpu: CPU): String = mnem("JMP") + "ea"
}

final class JSR(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val target = cpu.address(mode, reg, 4)
    cpu.pushInt(cpu.PC)
    cpu.jumpTo(target)
  }
  def disassemble(cpu: CPU): String = mnem("JSR") + "ea"
}

object RTS extends Instruction {
  def apply(cpu: CPU): Unit = cpu.jumpTo(cpu.popInt() & MAX_ADDRESS)
  def disassemble(cpu: CPU): String = "RTS"
}

object RTR extends Instruction {
  def apply(cpu: CPU): Unit = {
    cpu.toCCR(cpu.popShort())
    cpu.jumpTo(cpu.popInt() & MAX_ADDRESS)
  }
  def disassemble(cpu: CPU): String = "RTR"
}

object RTE extends Instruction {
  def apply(cpu: CPU): Unit =
    if cpu.supervisor then {
      val sr   = cpu.popShort() & 0xFFFF
      val newPC = cpu.popInt() & MAX_ADDRESS
      cpu.toSR(sr)
      cpu.jumpTo(newPC)
    }
  def disassemble(cpu: CPU): String = "RTE"
}

// ============================================================================
// Bit instructions — BCHG / BCLR / BSET / BTST
// `breg = -1` ⇒ immediate bit number; `breg ≥ 0` ⇒ bit number from Dn.
// Bit-wide ops are byte-wide on memory destinations (`mode > 1` = memory) and
// long-wide on Dn.
// ============================================================================

final class BCHG(breg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val rawBit =
      if breg < 0 then cpu.fetchShort() & 0xFF
      else cpu.D(breg)
    if mode == DataRegisterDirect then {
      val b = rawBit & 31
      val v = cpu.D(reg)
      cpu.setZ((v & (1 << b)) == 0)
      cpu.D(reg) = v ^ (1 << b)
    } else {
      val b = rawBit & 7
      cpu.readWriteByte(mode, reg) { v =>
        cpu.setZ((v & (1 << b)) == 0)
        v ^ (1 << b)
      }
    }
  }
  def disassemble(cpu: CPU): String = mnem("BCHG") + (if breg < 0 then "#imm" else s"D$breg") + ", ea"
}

final class BCLR(breg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val rawBit = if breg < 0 then cpu.fetchShort() & 0xFF else cpu.D(breg)
    if mode == DataRegisterDirect then {
      val b = rawBit & 31
      val v = cpu.D(reg)
      cpu.setZ((v & (1 << b)) == 0)
      cpu.D(reg) = v & ~(1 << b)
    } else {
      val b = rawBit & 7
      cpu.readWriteByte(mode, reg) { v =>
        cpu.setZ((v & (1 << b)) == 0)
        v & ~(1 << b)
      }
    }
  }
  def disassemble(cpu: CPU): String = mnem("BCLR") + (if breg < 0 then "#imm" else s"D$breg") + ", ea"
}

final class BSET(breg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val rawBit = if breg < 0 then cpu.fetchShort() & 0xFF else cpu.D(breg)
    if mode == DataRegisterDirect then {
      val b = rawBit & 31
      val v = cpu.D(reg)
      cpu.setZ((v & (1 << b)) == 0)
      cpu.D(reg) = v | (1 << b)
    } else {
      val b = rawBit & 7
      cpu.readWriteByte(mode, reg) { v =>
        cpu.setZ((v & (1 << b)) == 0)
        v | (1 << b)
      }
    }
  }
  def disassemble(cpu: CPU): String = mnem("BSET") + (if breg < 0 then "#imm" else s"D$breg") + ", ea"
}

final class BTST(breg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val rawBit = if breg < 0 then cpu.fetchShort() & 0xFF else cpu.D(breg)
    if mode == DataRegisterDirect then {
      val b = rawBit & 31
      cpu.setZ((cpu.D(reg) & (1 << b)) == 0)
    } else {
      val b = rawBit & 7
      val v = cpu.readByte(mode, reg) & 0xFF
      cpu.setZ((v & (1 << b)) == 0)
    }
  }
  def disassemble(cpu: CPU): String = mnem("BTST") + (if breg < 0 then "#imm" else s"D$breg") + ", ea"
}

// ============================================================================
// Misc / control
// ============================================================================

/** LEA — load effective address into An (no memory access on the source). */
final class LEA(areg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.writeA(areg, cpu.address(mode, reg, 4))
  def disassemble(cpu: CPU): String = mnem("LEA") + s"ea, A$areg"
}

/** PEA — push effective address onto the stack. */
final class PEA(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.pushInt(cpu.address(mode, reg, 4))
  def disassemble(cpu: CPU): String = mnem("PEA") + "ea"
}

/** LINK An,#disp — push An, set An to SP, add disp to SP. */
final class LINK(reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    cpu.pushInt(cpu.readA(reg))
    val sp = cpu.readA(7)
    cpu.writeA(reg, sp)
    val disp = (cpu.fetchShort() << 16) >> 16
    cpu.writeA(7, sp + disp)
  }
  def disassemble(cpu: CPU): String = mnem("LINK") + s"A$reg, #disp"
}

final class UNLK(reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    cpu.writeA(7, cpu.readA(reg))
    val v = cpu.popInt()
    cpu.writeA(reg, v)
  }
  def disassemble(cpu: CPU): String = mnem("UNLK") + s"A$reg"
}

/** TAS — test ea byte then set its high bit (an atomic-RMW primitive on real hardware). */
final class TAS(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteByte(mode, reg) { v =>
    cpu.ccr = (cpu.ccr & CCR.X) | (if v == 0 then CCR.Z else 0) | (if (v & 0x80) != 0 then CCR.N else 0)
    v | 0x80
  }
  def disassemble(cpu: CPU): String = mnem("TAS") + "ea"
}

/** CHK Dn against EA — if Dn < 0 or Dn > ea, take the CHK exception. */
final class CHKShort(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val upper = cpu.readShort(mode, reg)
    val d     = cpu.readDShort(dreg)
    if d < 0 then {
      cpu.setN(true); cpu.exception(-1, VectorTable.CHKInstruction)
    } else if d > upper then {
      cpu.setN(false); cpu.exception(-1, VectorTable.CHKInstruction)
    }
  }
  def disassemble(cpu: CPU): String = mnem("CHK", 'W')
}

final class CHKInt(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val upper = cpu.readInt(mode, reg)
    val d     = cpu.readDInt(dreg)
    if d < 0 then {
      cpu.setN(true); cpu.exception(-1, VectorTable.CHKInstruction)
    } else if d > upper then {
      cpu.setN(false); cpu.exception(-1, VectorTable.CHKInstruction)
    }
  }
  def disassemble(cpu: CPU): String = mnem("CHK", 'L')
}

/** EXG — exchange two registers. The dispatch table sets `mode` to 8 (Dn,Dn), 9 (An,An), or 17 (Dn,An). */
final class EXG(rx: Int, mode: Int, ry: Int) extends Instruction {
  def apply(cpu: CPU): Unit = mode match {
    case 0x08 => val t = cpu.D(rx); cpu.D(rx) = cpu.D(ry); cpu.D(ry) = t
    case 0x09 => val t = cpu.readA(rx); cpu.writeA(rx, cpu.readA(ry)); cpu.writeA(ry, t)
    case 0x11 => val t = cpu.D(rx); cpu.D(rx) = cpu.readA(ry); cpu.writeA(ry, t)
  }
  def disassemble(cpu: CPU): String = mnem("EXG") + (mode match {
    case 0x08 => s"D$rx, D$ry"; case 0x09 => s"A$rx, A$ry"; case 0x11 => s"D$rx, A$ry"
  })
}

final class TRAP(vector: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if !cpu.trap(vector) then cpu.exception(-1, VectorTable.TRAPInstruction + (vector << 2))
  def disassemble(cpu: CPU): String = mnem("TRAP") + s"#$vector"
}

object TRAPV extends Instruction {
  def apply(cpu: CPU): Unit = if cpu.V then cpu.exception(-1, VectorTable.TRAPVInstruction)
  def disassemble(cpu: CPU): String = "TRAPV"
}

object RESET extends Instruction {
  def apply(cpu: CPU): Unit = if cpu.supervisor then cpu.resetSignal()
  def disassemble(cpu: CPU): String = "RESET"
}

object STOP extends Instruction {
  def apply(cpu: CPU): Unit =
    if cpu.supervisor then {
      cpu.toSR(cpu.fetchShort())
      cpu.stopped = true
    }
  def disassemble(cpu: CPU): String = mnem("STOP") + "#imm"
}

final class BKPT(bkpt: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if !cpu.breakpoint(bkpt) then cpu.exception(-1, VectorTable.illegalInstruction)
  def disassemble(cpu: CPU): String = mnem("BKPT") + s"#$bkpt"
}

object LINEA extends Instruction {
  def apply(cpu: CPU): Unit = if !cpu.lineA then cpu.exception(-1, VectorTable.lineA)
  def disassemble(cpu: CPU): String = "LINEA"
}

object LINEF extends Instruction {
  def apply(cpu: CPU): Unit = if !cpu.lineF then cpu.exception(-1, VectorTable.lineF)
  def disassemble(cpu: CPU): String = "LINEF"
}

// ============================================================================
// Shifts and rotates — register form (count or Dn, ALU helper)
// `ir == 0` ⇒ count is a 3-bit immediate (1..8, with 0 meaning 8); `ir == 1` ⇒ count is the low 6 bits of Dn.
// ============================================================================

private inline def shiftCountByteOrShort(cpu: CPU, ir: Int, count: Int): Int =
  if ir == 0 then (if count == 0 then 8 else count) else cpu.D(count) & 63

final class ASLByteReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDByte(dreg, cpu.aslByte(c, cpu.readDByte(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ASL", 'B')
}
final class ASLShortReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDShort(dreg, cpu.aslShort(c, cpu.readDShort(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ASL", 'W')
}
final class ASLIntReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDInt(dreg, cpu.aslInt(c, cpu.D(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ASL", 'L')
}

final class ASRByteReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDByte(dreg, cpu.asrByte(c, cpu.readDByte(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ASR", 'B')
}
final class ASRShortReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDShort(dreg, cpu.asrShort(c, cpu.readDShort(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ASR", 'W')
}
final class ASRIntReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDInt(dreg, cpu.asrInt(c, cpu.D(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ASR", 'L')
}

final class LSLByteReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDByte(dreg, cpu.lslByte(c, cpu.readDByte(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("LSL", 'B')
}
final class LSLShortReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDShort(dreg, cpu.lslShort(c, cpu.readDShort(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("LSL", 'W')
}
final class LSLIntReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDInt(dreg, cpu.lslInt(c, cpu.D(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("LSL", 'L')
}

final class LSRByteReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDByte(dreg, cpu.lsrByte(c, cpu.readDByte(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("LSR", 'B')
}
final class LSRShortReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDShort(dreg, cpu.lsrShort(c, cpu.readDShort(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("LSR", 'W')
}
final class LSRIntReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDInt(dreg, cpu.lsrInt(c, cpu.D(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("LSR", 'L')
}

final class ROLByteReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDByte(dreg, cpu.rolByte(c, cpu.readDByte(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ROL", 'B')
}
final class ROLShortReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDShort(dreg, cpu.rolShort(c, cpu.readDShort(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ROL", 'W')
}
final class ROLIntReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDInt(dreg, cpu.rolInt(c, cpu.D(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ROL", 'L')
}

final class RORByteReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDByte(dreg, cpu.rorByte(c, cpu.readDByte(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ROR", 'B')
}
final class RORShortReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDShort(dreg, cpu.rorShort(c, cpu.readDShort(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ROR", 'W')
}
final class RORIntReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDInt(dreg, cpu.rorInt(c, cpu.D(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ROR", 'L')
}

final class ROXLByteReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDByte(dreg, cpu.roxlByte(c, cpu.readDByte(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ROXL", 'B')
}
final class ROXLShortReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDShort(dreg, cpu.roxlShort(c, cpu.readDShort(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ROXL", 'W')
}
final class ROXLIntReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDInt(dreg, cpu.roxlInt(c, cpu.D(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ROXL", 'L')
}

final class ROXRByteReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDByte(dreg, cpu.roxrByte(c, cpu.readDByte(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ROXR", 'B')
}
final class ROXRShortReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDShort(dreg, cpu.roxrShort(c, cpu.readDShort(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ROXR", 'W')
}
final class ROXRIntReg(count: Int, ir: Int, dreg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val c = shiftCountByteOrShort(cpu, ir, count)
    cpu.writeDInt(dreg, cpu.roxrInt(c, cpu.D(dreg)))
  }
  def disassemble(cpu: CPU): String = mnem("ROXR", 'L')
}

/** Memory shift forms — always one-bit, always word-wide. */
final class ASLMem(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteShort(mode, reg)(d => cpu.aslShort(1, d))
  def disassemble(cpu: CPU): String = mnem("ASL", 'W')
}
final class ASRMem(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteShort(mode, reg)(d => cpu.asrShort(1, d))
  def disassemble(cpu: CPU): String = mnem("ASR", 'W')
}
final class LSLMem(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteShort(mode, reg)(d => cpu.lslShort(1, d))
  def disassemble(cpu: CPU): String = mnem("LSL", 'W')
}
final class LSRMem(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteShort(mode, reg)(d => cpu.lsrShort(1, d))
  def disassemble(cpu: CPU): String = mnem("LSR", 'W')
}
final class ROLMem(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteShort(mode, reg)(d => cpu.rolShort(1, d))
  def disassemble(cpu: CPU): String = mnem("ROL", 'W')
}
final class RORMem(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteShort(mode, reg)(d => cpu.rorShort(1, d))
  def disassemble(cpu: CPU): String = mnem("ROR", 'W')
}
final class ROXLMem(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteShort(mode, reg)(d => cpu.roxlShort(1, d))
  def disassemble(cpu: CPU): String = mnem("ROXL", 'W')
}
final class ROXRMem(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteShort(mode, reg)(d => cpu.roxrShort(1, d))
  def disassemble(cpu: CPU): String = mnem("ROXR", 'W')
}

// ============================================================================
// Multiply / divide
// ============================================================================

/** MULS — 16×16 → 32 signed, low 16 of Dn × ea word, full Dn := result. */
final class MULS(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val res = cpu.readShort(mode, reg) * cpu.readDShort(dreg)
    cpu.D(dreg) = res
    cpu.tstInt(res)
  }
  def disassemble(cpu: CPU): String = mnem("MULS", 'W')
}

/** MULU — 16×16 → 32 unsigned. */
final class MULU(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val a   = cpu.readShort(mode, reg) & 0xFFFF
    val b   = cpu.readDShort(dreg) & 0xFFFF
    val res = a * b
    cpu.D(dreg) = res
    cpu.tstInt(res)
  }
  def disassemble(cpu: CPU): String = mnem("MULU", 'W')
}

/** DIVS — 32 / 16 → 16 quotient (low Dn) + 16 remainder (high Dn). */
final class DIVS(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val divisor = cpu.readShort(mode, reg)
    if divisor == 0 then cpu.exception(-1, VectorTable.integerDivideByZero)
    else {
      val a = cpu.D(dreg)
      val q = a / divisor
      if q < Short.MinValue || q > Short.MaxValue then cpu.setV(true)
      else {
        val r = a % divisor
        cpu.D(dreg) = ((r & 0xFFFF) << 16) | (q & 0xFFFF)
        cpu.ccr = (cpu.ccr & CCR.X) | (if q == 0 then CCR.Z else 0) | (if q < 0 then CCR.N else 0)
      }
    }
  }
  def disassemble(cpu: CPU): String = mnem("DIVS", 'W')
}

final class DIVU(dreg: Int, mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = {
    val divisor = cpu.readShort(mode, reg) & 0xFFFF
    if divisor == 0 then cpu.exception(-1, VectorTable.integerDivideByZero)
    else {
      val a = cpu.D(dreg).toLong & 0xFFFFFFFFL
      val q = a / divisor
      if q > 0xFFFFL then cpu.setV(true)
      else {
        val r = (a % divisor).toInt
        val qi = q.toInt
        cpu.D(dreg) = ((r & 0xFFFF) << 16) | (qi & 0xFFFF)
        cpu.ccr = (cpu.ccr & CCR.X) | (if qi == 0 then CCR.Z else 0) | (if (qi & 0x8000) != 0 then CCR.N else 0)
      }
    }
  }
  def disassemble(cpu: CPU): String = mnem("DIVU", 'W')
}

// ============================================================================
// BCD instructions
// ============================================================================

/** ABCD — Dn,Dn (`r == 0`) or -(Ay),-(Ax) (`r == 1`). */
final class ABCD(y: Int, r: Int, x: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if r == 0 then cpu.writeDByte(x, cpu.abcd(cpu.readDByte(y), cpu.readDByte(x)))
    else {
      val sAddr = cpu.predecrementA(y, 1); val s = cpu.memory.readByte(sAddr)
      val dAddr = cpu.predecrementA(x, 1); val d = cpu.memory.readByte(dAddr)
      cpu.memory.writeByte(dAddr, cpu.abcd(s, d))
    }
  def disassemble(cpu: CPU): String =
    mnem("ABCD") + (if r == 0 then s"D$y, D$x" else s"-(A$y), -(A$x)")
}

final class SBCD(y: Int, r: Int, x: Int) extends Instruction {
  def apply(cpu: CPU): Unit =
    if r == 0 then cpu.writeDByte(x, cpu.sbcd(cpu.readDByte(y), cpu.readDByte(x)))
    else {
      val sAddr = cpu.predecrementA(y, 1); val s = cpu.memory.readByte(sAddr)
      val dAddr = cpu.predecrementA(x, 1); val d = cpu.memory.readByte(dAddr)
      cpu.memory.writeByte(dAddr, cpu.sbcd(s, d))
    }
  def disassemble(cpu: CPU): String =
    mnem("SBCD") + (if r == 0 then s"D$y, D$x" else s"-(A$y), -(A$x)")
}

final class NBCD(mode: Int, reg: Int) extends Instruction {
  def apply(cpu: CPU): Unit = cpu.readWriteByte(mode, reg)(d => cpu.nbcd(d))
  def disassemble(cpu: CPU): String = mnem("NBCD") + "ea"
}
