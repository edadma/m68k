package io.github.edadma.m68k

import java.io.PrintStream

import scala.collection.mutable.{HashMap, ListBuffer}

import Addressing.*

/** Motorola 68000 core.
  *
  * Design notes (these depart from the original implementation deliberately; see CLAUDE.md history if curious):
  *
  *   - **Flags as a single Int**, not five booleans. `ccr` holds X/N/Z/V/C in the same bit positions the m68k uses
  *     in the SR low byte, so SR push/pop is one mask, not a five-field recombine. Per-op flag updates are also
  *     one masked OR instead of five branchful boolean stores.
  *   - **No Size trait.** The original parameterised every read/write/ALU helper by a `Size` ADT and then matched
  *     on it inside the hot path. Here every operation has three width-specialised methods (`Byte`/`Short`/`Int`).
  *     The dispatch happens once at decode time when the assembler picks which method to call, not on every
  *     execute.
  *   - **Inline read-modify-write.** `readWriteByte/Short/Int` are `inline def`s with `inline op: Int => Int`.
  *     The compiler erases the lambda — each instruction body that uses one ends up with a direct ALU call
  *     spliced into the dispatch, no Function1 allocation per cycle.
  *
  * The `Memory` already does paged dispatch + last-region caching, so the dominant per-cycle costs in this CPU
  * are: one fetch, one opcode-table index, one instruction `apply`, and the work the instruction itself does.
  */
class CPU(private[m68k] val memory: Memory) {

  // ----------------------------------------------------------------------------
  // Architectural state
  // ----------------------------------------------------------------------------

  private[m68k] val D    = new Array[Int](8)
  private[m68k] val A    = new Array[Int](7)
  private[m68k] var PC   = 0
  private[m68k] var USP  = 0
  private[m68k] var SSP  = 0

  /** Status register, high byte (T, S, M, I).
    *
    * Holds bits 8..15 only — the low byte (CCR) lives in `ccr` for fast manipulation. `fromSR` recombines them.
    */
  private[m68k] var sr: Int = 0

  /** Condition Code Register (X N Z V C), packed into one Int at the m68k SR-low-byte positions. */
  private[m68k] var ccr: Int = 0

  private[m68k] var instruction = 0
  private[m68k] var prog: Addressable = null

  // Run-loop / debug bookkeeping.
  private val resettables = new ListBuffer[Device]
  protected[m68k] var running = false
  protected[m68k] var stopped = false

  /** Breakpoint table — value `true` marks single-shot (cleared after firing). */
  private[m68k] val breakpointMap = new HashMap[Int, Boolean]

  /** Trace mode: when on, the CPU prints registers + the disassembled instruction after every step. */
  var trace: Boolean         = false
  var traceOut: PrintStream  = Console.out

  /** Reverse symbol table — address → label, populated by tooling that loads symbol info. */
  var reverseSymbols: Map[Int, String] = Map.empty

  def resettable(dev: Device): Unit = resettables += dev
  def isRunning: Boolean            = running

  def isBreakpoint(addr: Int): Boolean       = breakpointMap.contains(addr)
  def setBreakpoint(addr: Int): Unit         = breakpointMap(addr) = false
  def setSingleShotBreakpoint(addr: Int): Unit = breakpointMap(addr) = true
  def clearBreakpoint(addr: Int): Unit       = breakpointMap.remove(addr)
  def clearBreakpoints(): Unit               = breakpointMap.clear()
  def breakpoints: List[Int]                 = breakpointMap.keysIterator.toList.sorted

  // ----------------------------------------------------------------------------
  // CCR helpers — one masked OR per flag update
  // ----------------------------------------------------------------------------

  inline def C: Boolean = (ccr & CCR.C) != 0
  inline def V: Boolean = (ccr & CCR.V) != 0
  inline def Z: Boolean = (ccr & CCR.Z) != 0
  inline def N: Boolean = (ccr & CCR.N) != 0
  inline def X: Boolean = (ccr & CCR.X) != 0

  inline def setC(b: Boolean): Unit = ccr = if b then ccr | CCR.C else ccr & ~CCR.C
  inline def setV(b: Boolean): Unit = ccr = if b then ccr | CCR.V else ccr & ~CCR.V
  inline def setZ(b: Boolean): Unit = ccr = if b then ccr | CCR.Z else ccr & ~CCR.Z
  inline def setN(b: Boolean): Unit = ccr = if b then ccr | CCR.N else ccr & ~CCR.N
  inline def setX(b: Boolean): Unit = ccr = if b then ccr | CCR.X else ccr & ~CCR.X

  /** SR with CCR merged in — the value the m68k actually presents to MOVE/ANDI/ORI/EORI from-SR. */
  def fromSR: Int = sr | (ccr & CCR.Mask)

  /** Load SR (writes both halves). */
  def toSR(bits: Int): Unit = {
    sr  = bits & 0xFF00
    ccr = bits & CCR.Mask
  }

  /** Load CCR (low byte only). */
  def toCCR(bits: Int): Unit = ccr = bits & CCR.Mask

  /** Internal convenience: write all five CCR bits in a single masked OR.
    *
    * `keepZ=true` implements the "extended" semantics of ADDX/SUBX/NEGX: Z stays unless the result is non-zero,
    * giving the multi-precision-arithmetic carry chain its expected behaviour.
    */
  inline private def writeCCR(c: Boolean, v: Boolean, z: Boolean, n: Boolean, x: Boolean, keepZ: Boolean = false): Unit = {
    val zEff = if keepZ then z && Z else z
    ccr = (if c then CCR.C else 0) |
          (if v then CCR.V else 0) |
          (if zEff then CCR.Z else 0) |
          (if n then CCR.N else 0) |
          (if x then CCR.X else 0)
  }

  /** Logic-op flag update (AND/OR/EOR/NOT/MOVE/MOVEQ/EXT/SWAP/TST): clears V/C, sets N/Z, leaves X. */
  inline private def setLogicFlagsByte(res: Int): Unit = {
    val r = res & 0xFF
    ccr = (ccr & CCR.X) | (if r == 0 then CCR.Z else 0) | (if (r & 0x80) != 0 then CCR.N else 0)
  }
  inline private def setLogicFlagsShort(res: Int): Unit = {
    val r = res & 0xFFFF
    ccr = (ccr & CCR.X) | (if r == 0 then CCR.Z else 0) | (if (r & 0x8000) != 0 then CCR.N else 0)
  }
  inline private def setLogicFlagsInt(res: Int): Unit =
    ccr = (ccr & CCR.X) | (if res == 0 then CCR.Z else 0) | (if res < 0 then CCR.N else 0)

  // ----------------------------------------------------------------------------
  // Reset
  // ----------------------------------------------------------------------------

  reset()

  def reset(): Unit = {
    stop()
    memory.reset()
    resetSignal()
    java.util.Arrays.fill(D, 0)
    java.util.Arrays.fill(A, 0)
    sr  = SRBit.S | SRBit.I
    ccr = 0
    SSP = memory.readInt(VectorTable.SSP)
    jumpTo(memory.readInt(VectorTable.PC))
  }

  def resetSignal(): Unit = resettables.foreach(_.reset())

  def stop(): Unit = {
    running = false
    stopped = false
  }

  def jumpTo(address: Int): Unit = {
    prog = memory.find(address)
    PC   = address
  }

  // ----------------------------------------------------------------------------
  // Fetch
  // ----------------------------------------------------------------------------

  inline def fetchShort(): Int = {
    val res = prog.readShort(PC)
    PC += 2
    res
  }

  inline def fetchInt(): Int = {
    val res = prog.readInt(PC)
    PC += 4
    res
  }

  /** Fetch a sign-extended 8-bit immediate from the low byte of an extension word. */
  inline def fetchByte(): Int = {
    val w = fetchShort()
    (w << 24) >> 24
  }

  inline def fetch(): Unit = instruction = fetchShort() & 0xFFFF

  // ----------------------------------------------------------------------------
  // Address-register access (A7 routes to USP/SSP based on supervisor mode)
  // ----------------------------------------------------------------------------

  inline def supervisorMode: Boolean = (sr & SRBit.S) != 0

  def readA(reg: Int): Int =
    if reg == 7 then (if supervisorMode then SSP else USP)
    else A(reg)

  def writeA(reg: Int, value: Int): Unit =
    if reg == 7 then { if supervisorMode then SSP = value else USP = value }
    else A(reg) = value

  /** Predecrement A7 stack pointer: byte-aligned operations on A7 force word-step (the m68k bumps A7 by 2 even
    * when the size is byte, to keep the stack aligned). */
  def predecrementA(reg: Int, sizeBytes: Int): Int = {
    val step = if reg == 7 && sizeBytes == 1 then 2 else sizeBytes
    val newV = readA(reg) - step
    writeA(reg, newV)
    newV
  }

  def postincrementA(reg: Int, sizeBytes: Int): Int = {
    val cur  = readA(reg)
    val step = if reg == 7 && sizeBytes == 1 then 2 else sizeBytes
    writeA(reg, cur + step)
    cur
  }

  // ----------------------------------------------------------------------------
  // Data-register read/write (size-specialised, no Size param)
  // ----------------------------------------------------------------------------

  /** Returns the raw 32-bit register value. */
  inline def readDInt(reg: Int): Int = D(reg)

  /** Returns the low 16 bits, sign-extended. */
  inline def readDShort(reg: Int): Int = (D(reg) << 16) >> 16

  /** Returns the low 8 bits, sign-extended. */
  inline def readDByte(reg: Int): Int = (D(reg) << 24) >> 24

  /** Write the full 32 bits. */
  inline def writeDInt(reg: Int, value: Int): Unit = D(reg) = value

  /** Write only the low 16 bits, leaving the upper 16 unchanged. */
  inline def writeDShort(reg: Int, value: Int): Unit =
    D(reg) = (D(reg) & 0xFFFF0000) | (value & 0xFFFF)

  /** Write only the low 8 bits, leaving the upper 24 unchanged. */
  inline def writeDByte(reg: Int, value: Int): Unit =
    D(reg) = (D(reg) & 0xFFFFFF00) | (value & 0xFF)

  // ----------------------------------------------------------------------------
  // Effective-address computation
  // ----------------------------------------------------------------------------

  /** Compute the EA for memory addressing modes (and only those — register-direct modes don't have an address).
    *
    * Has side effects on PC (consumes extension words) and on the address register itself for predec/postinc, so
    * call exactly once per instruction execution per operand.
    */
  def address(mode: Int, reg: Int, sizeBytes: Int): Int = mode match {
    case AddressRegisterIndirect                    => readA(reg)
    case AddressRegisterIndirectPostincrement       => postincrementA(reg, sizeBytes)
    case AddressRegisterIndirectPredecrement        => predecrementA(reg, sizeBytes)
    case AddressRegisterIndirectWithDisplacement    => readA(reg) + ((fetchShort() << 16) >> 16)
    case AddressRegisterIndirectWithIndex           => indexed(readA(reg))
    case OtherModes => reg match {
      case AbsoluteShort                  => (fetchShort() << 16) >> 16   // sign-extended to 32
      case AbsoluteLong                   => fetchInt()
      case ProgramCounterWithDisplacement =>
        val pc = PC
        pc + ((fetchShort() << 16) >> 16)
      case ProgramCounterWithIndex =>
        val pc = PC
        indexed(pc)
    }
  }

  private def indexed(base: Int): Int = {
    val ext  = fetchShort()
    val r    = (ext >> 12) & 7
    val long = (ext & 0x0800) != 0
    val isA  = (ext & 0x8000) != 0
    val idx  =
      if isA then { val a = readA(r); if long then a else (a << 16) >> 16 }
      else        { if long then D(r) else readDShort(r) }
    val disp = (ext << 24) >> 24 // sign-extended displacement byte
    base + idx + disp
  }

  // ----------------------------------------------------------------------------
  // Effective-address read (size-specialised)
  // ----------------------------------------------------------------------------

  /** Read an 8-bit operand, sign-extended to Int. */
  def readByte(mode: Int, reg: Int): Int = mode match {
    case DataRegisterDirect    => readDByte(reg)
    case AddressRegisterDirect => (readA(reg) << 24) >> 24
    case OtherModes if reg == ImmediateData => fetchByte()
    case _ =>
      val addr = address(mode, reg, 1)
      val b    = memory.readByte(addr)
      (b << 24) >> 24
  }

  def readShort(mode: Int, reg: Int): Int = mode match {
    case DataRegisterDirect    => readDShort(reg)
    case AddressRegisterDirect => (readA(reg) << 16) >> 16
    case OtherModes if reg == ImmediateData => (fetchShort() << 16) >> 16
    case _ =>
      val addr = address(mode, reg, 2)
      val s    = memory.readShort(addr)
      (s << 16) >> 16
  }

  def readInt(mode: Int, reg: Int): Int = mode match {
    case DataRegisterDirect    => D(reg)
    case AddressRegisterDirect => readA(reg)
    case OtherModes if reg == ImmediateData => fetchInt()
    case _ =>
      val addr = address(mode, reg, 4)
      memory.readInt(addr)
  }

  // ----------------------------------------------------------------------------
  // Effective-address write (size-specialised)
  // ----------------------------------------------------------------------------

  def writeByte(mode: Int, reg: Int, value: Int): Unit = mode match {
    case DataRegisterDirect    => writeDByte(reg, value)
    case AddressRegisterDirect => writeA(reg, (readA(reg) & 0xFFFFFF00) | (value & 0xFF))
    case _ =>
      val addr = address(mode, reg, 1)
      memory.writeByte(addr, value)
  }

  def writeShort(mode: Int, reg: Int, value: Int): Unit = mode match {
    case DataRegisterDirect    => writeDShort(reg, value)
    case AddressRegisterDirect => writeA(reg, (readA(reg) & 0xFFFF0000) | (value & 0xFFFF))
    case _ =>
      val addr = address(mode, reg, 2)
      memory.writeShort(addr, value)
  }

  def writeInt(mode: Int, reg: Int, value: Int): Unit = mode match {
    case DataRegisterDirect    => writeDInt(reg, value)
    case AddressRegisterDirect => writeA(reg, value)
    case _ =>
      val addr = address(mode, reg, 4)
      memory.writeInt(addr, value)
  }

  // ----------------------------------------------------------------------------
  // Inline read-modify-write — the original used a closure (`Int => Int`) per RMW
  // instruction. With Scala 3 `inline def` + `inline op`, the compiler splices the
  // ALU call directly into the body — no Function1 allocated per cycle.
  // ----------------------------------------------------------------------------

  inline def readWriteByte(mode: Int, reg: Int)(inline op: Int => Int): Unit = mode match {
    case DataRegisterDirect    => writeDByte(reg, op(readDByte(reg)))
    case AddressRegisterDirect => writeA(reg, (readA(reg) & 0xFFFFFF00) | (op((readA(reg) << 24) >> 24) & 0xFF))
    case _ =>
      val addr = address(mode, reg, 1)
      memory.writeByte(addr, op((memory.readByte(addr) << 24) >> 24))
  }

  inline def readWriteShort(mode: Int, reg: Int)(inline op: Int => Int): Unit = mode match {
    case DataRegisterDirect    => writeDShort(reg, op(readDShort(reg)))
    case AddressRegisterDirect => writeA(reg, (readA(reg) & 0xFFFF0000) | (op((readA(reg) << 16) >> 16) & 0xFFFF))
    case _ =>
      val addr = address(mode, reg, 2)
      memory.writeShort(addr, op((memory.readShort(addr) << 16) >> 16))
  }

  inline def readWriteInt(mode: Int, reg: Int)(inline op: Int => Int): Unit = mode match {
    case DataRegisterDirect    => D(reg) = op(D(reg))
    case AddressRegisterDirect => writeA(reg, op(readA(reg)))
    case _ =>
      val addr = address(mode, reg, 4)
      memory.writeInt(addr, op(memory.readInt(addr)))
  }

  // ----------------------------------------------------------------------------
  // Stack
  // ----------------------------------------------------------------------------

  def pushShort(value: Int): Unit = {
    val sp = predecrementA(7, 2)
    memory.writeShort(sp, value)
  }

  def pushInt(value: Int): Unit = {
    val sp = predecrementA(7, 4)
    memory.writeInt(sp, value)
  }

  def popShort(): Int = {
    val sp = postincrementA(7, 2)
    (memory.readShort(sp) << 16) >> 16
  }

  def popInt(): Int = {
    val sp = postincrementA(7, 4)
    memory.readInt(sp)
  }

  /** Address pushed by JSR/BSR/exception — the return target on the top of the stack. */
  def subroutineReturnAddress: Int = memory.readInt(readA(7)) & MAX_ADDRESS

  /** Return address from an exception frame (skips the SR word). */
  def exceptionReturnAddress: Int = memory.readInt(readA(7) + 2) & MAX_ADDRESS

  // ----------------------------------------------------------------------------
  // ALU — width-specialised (no Size param matched on per call)
  //
  // Carry/overflow formulas come straight from the m68k reference manual; we keep
  // the same expressions the original used, just unfolded to three explicit width
  // variants so the bit masks are constants the JIT can fold.
  // ----------------------------------------------------------------------------

  // ADD --------------------------------------------------------------------

  def addByte(s: Int, d: Int): Int = {
    val ss = s & 0xFF
    val dd = d & 0xFF
    val r  = (ss + dd) & 0xFF
    val c  = ((ss & dd) | (~r & (ss | dd))) & 0x80
    val v  = ((ss & dd & ~r) | (~ss & ~dd & r)) & 0x80
    val cb = c != 0
    writeCCR(c = cb, v = v != 0, z = r == 0, n = (r & 0x80) != 0, x = cb)
    r
  }

  def addShort(s: Int, d: Int): Int = {
    val ss = s & 0xFFFF
    val dd = d & 0xFFFF
    val r  = (ss + dd) & 0xFFFF
    val c  = ((ss & dd) | (~r & (ss | dd))) & 0x8000
    val v  = ((ss & dd & ~r) | (~ss & ~dd & r)) & 0x8000
    val cb = c != 0
    writeCCR(c = cb, v = v != 0, z = r == 0, n = (r & 0x8000) != 0, x = cb)
    r
  }

  def addInt(s: Int, d: Int): Int = {
    val r  = s + d
    val c  = ((s.toLong & 0xFFFFFFFFL) + (d.toLong & 0xFFFFFFFFL)) > 0xFFFFFFFFL
    val v  = ((s & d & ~r) | (~s & ~d & r)) < 0
    writeCCR(c = c, v = v, z = r == 0, n = r < 0, x = c)
    r
  }

  // ADDX -------------------------------------------------------------------

  def addxByte(s: Int, d: Int): Int = {
    val xb = if X then 1 else 0
    val ss = s & 0xFF
    val dd = d & 0xFF
    val sum = ss + dd + xb
    val r  = sum & 0xFF
    val c  = sum > 0xFF
    val v  = ((ss & dd & ~r) | (~ss & ~dd & r)) & 0x80
    writeCCR(c = c, v = v != 0, z = r == 0, n = (r & 0x80) != 0, x = c, keepZ = true)
    r
  }

  def addxShort(s: Int, d: Int): Int = {
    val xb = if X then 1 else 0
    val ss = s & 0xFFFF
    val dd = d & 0xFFFF
    val sum = ss + dd + xb
    val r  = sum & 0xFFFF
    val c  = sum > 0xFFFF
    val v  = ((ss & dd & ~r) | (~ss & ~dd & r)) & 0x8000
    writeCCR(c = c, v = v != 0, z = r == 0, n = (r & 0x8000) != 0, x = c, keepZ = true)
    r
  }

  def addxInt(s: Int, d: Int): Int = {
    val xb = if X then 1L else 0L
    val sum = (s.toLong & 0xFFFFFFFFL) + (d.toLong & 0xFFFFFFFFL) + xb
    val r   = sum.toInt
    val c   = sum > 0xFFFFFFFFL
    val v   = ((s & d & ~r) | (~s & ~d & r)) < 0
    writeCCR(c = c, v = v, z = r == 0, n = r < 0, x = c, keepZ = true)
    r
  }

  // SUB --------------------------------------------------------------------

  def subByte(s: Int, d: Int): Int = {
    val ss = s & 0xFF
    val dd = d & 0xFF
    val r  = (dd - ss) & 0xFF
    val c  = ((ss & ~dd) | (r & ~dd) | (s & r)) & 0x80
    val v  = ((~ss & dd & ~r) | (ss & ~dd & r)) & 0x80
    val cb = c != 0
    writeCCR(c = cb, v = v != 0, z = r == 0, n = (r & 0x80) != 0, x = cb)
    r
  }

  def subShort(s: Int, d: Int): Int = {
    val ss = s & 0xFFFF
    val dd = d & 0xFFFF
    val r  = (dd - ss) & 0xFFFF
    val c  = ((ss & ~dd) | (r & ~dd) | (s & r)) & 0x8000
    val v  = ((~ss & dd & ~r) | (ss & ~dd & r)) & 0x8000
    val cb = c != 0
    writeCCR(c = cb, v = v != 0, z = r == 0, n = (r & 0x8000) != 0, x = cb)
    r
  }

  def subInt(s: Int, d: Int): Int = {
    val r = d - s
    val c = (s.toLong & 0xFFFFFFFFL) > (d.toLong & 0xFFFFFFFFL)
    val v = ((~s & d & ~r) | (s & ~d & r)) < 0
    writeCCR(c = c, v = v, z = r == 0, n = r < 0, x = c)
    r
  }

  // SUBX -------------------------------------------------------------------

  def subxByte(s: Int, d: Int): Int = {
    val xb = if X then 1 else 0
    val ss = s & 0xFF
    val dd = d & 0xFF
    val diff = dd - ss - xb
    val r    = diff & 0xFF
    val c    = diff < 0
    val v    = ((~ss & dd & ~r) | (ss & ~dd & r)) & 0x80
    writeCCR(c = c, v = v != 0, z = r == 0, n = (r & 0x80) != 0, x = c, keepZ = true)
    r
  }

  def subxShort(s: Int, d: Int): Int = {
    val xb = if X then 1 else 0
    val ss = s & 0xFFFF
    val dd = d & 0xFFFF
    val diff = dd - ss - xb
    val r    = diff & 0xFFFF
    val c    = diff < 0
    val v    = ((~ss & dd & ~r) | (ss & ~dd & r)) & 0x8000
    writeCCR(c = c, v = v != 0, z = r == 0, n = (r & 0x8000) != 0, x = c, keepZ = true)
    r
  }

  def subxInt(s: Int, d: Int): Int = {
    val xb = if X then 1L else 0L
    val diff = (d.toLong & 0xFFFFFFFFL) - (s.toLong & 0xFFFFFFFFL) - xb
    val r    = diff.toInt
    val c    = diff < 0
    val v    = ((~s & d & ~r) | (s & ~d & r)) < 0
    writeCCR(c = c, v = v, z = r == 0, n = r < 0, x = c, keepZ = true)
    r
  }

  // CMP -- like SUB but doesn't write the result, doesn't touch X --------------

  def cmpByte(s: Int, d: Int): Unit = {
    val ss = s & 0xFF
    val dd = d & 0xFF
    val r  = (dd - ss) & 0xFF
    val c  = ((ss & ~dd) | (r & ~dd) | (s & r)) & 0x80
    val v  = ((~ss & dd & ~r) | (ss & ~dd & r)) & 0x80
    ccr = (ccr & CCR.X) |
      (if c != 0 then CCR.C else 0) |
      (if v != 0 then CCR.V else 0) |
      (if r == 0 then CCR.Z else 0) |
      (if (r & 0x80) != 0 then CCR.N else 0)
  }

  def cmpShort(s: Int, d: Int): Unit = {
    val ss = s & 0xFFFF
    val dd = d & 0xFFFF
    val r  = (dd - ss) & 0xFFFF
    val c  = ((ss & ~dd) | (r & ~dd) | (s & r)) & 0x8000
    val v  = ((~ss & dd & ~r) | (ss & ~dd & r)) & 0x8000
    ccr = (ccr & CCR.X) |
      (if c != 0 then CCR.C else 0) |
      (if v != 0 then CCR.V else 0) |
      (if r == 0 then CCR.Z else 0) |
      (if (r & 0x8000) != 0 then CCR.N else 0)
  }

  def cmpInt(s: Int, d: Int): Unit = {
    val r = d - s
    val c = (s.toLong & 0xFFFFFFFFL) > (d.toLong & 0xFFFFFFFFL)
    val v = ((~s & d & ~r) | (s & ~d & r)) < 0
    ccr = (ccr & CCR.X) |
      (if c then CCR.C else 0) |
      (if v then CCR.V else 0) |
      (if r == 0 then CCR.Z else 0) |
      (if r < 0 then CCR.N else 0)
  }

  // NEG --------------------------------------------------------------------

  def negByte(d: Int): Int = {
    val dd = d & 0xFF
    val r  = (-dd) & 0xFF
    val c  = dd != 0
    val v  = (dd & r & 0x80) != 0
    writeCCR(c = c, v = v, z = r == 0, n = (r & 0x80) != 0, x = c)
    r
  }

  def negShort(d: Int): Int = {
    val dd = d & 0xFFFF
    val r  = (-dd) & 0xFFFF
    val c  = dd != 0
    val v  = (dd & r & 0x8000) != 0
    writeCCR(c = c, v = v, z = r == 0, n = (r & 0x8000) != 0, x = c)
    r
  }

  def negInt(d: Int): Int = {
    val r = -d
    val c = d != 0
    val v = (d & r) < 0   // both have sign bit ⇒ overflow (only true for MIN_VALUE)
    writeCCR(c = c, v = v, z = r == 0, n = r < 0, x = c)
    r
  }

  // NEGX -------------------------------------------------------------------

  def negxByte(d: Int): Int = {
    val xb = if X then 1 else 0
    val dd = d & 0xFF
    val diff = -dd - xb
    val r    = diff & 0xFF
    val c    = (dd | xb) != 0 && (dd + xb) != 0  // same as (-dd-xb) borrows
    val cBor = diff < 0 || (xb == 1 && dd == 0xFF)  // simpler: borrow iff signed diff < 0
    val cFinal = diff < 0  // borrow when result would be negative
    val v    = (dd & r & 0x80) != 0
    writeCCR(c = cFinal, v = v, z = r == 0, n = (r & 0x80) != 0, x = cFinal, keepZ = true)
    r
  }

  def negxShort(d: Int): Int = {
    val xb = if X then 1 else 0
    val dd = d & 0xFFFF
    val diff = -dd - xb
    val r    = diff & 0xFFFF
    val c    = diff < 0
    val v    = (dd & r & 0x8000) != 0
    writeCCR(c = c, v = v, z = r == 0, n = (r & 0x8000) != 0, x = c, keepZ = true)
    r
  }

  def negxInt(d: Int): Int = {
    val xb = if X then 1L else 0L
    val diff = -(d.toLong & 0xFFFFFFFFL) - xb
    val r    = diff.toInt
    val c    = diff < 0
    val v    = (d & r) < 0
    writeCCR(c = c, v = v, z = r == 0, n = r < 0, x = c, keepZ = true)
    r
  }

  // NOT, AND, OR, EOR, CLR, TST, MOVE -------------------------------------

  def notByte(d: Int): Int  = { val r = (~d) & 0xFF;   setLogicFlagsByte(r);  r }
  def notShort(d: Int): Int = { val r = (~d) & 0xFFFF; setLogicFlagsShort(r); r }
  def notInt(d: Int): Int   = { val r = ~d;            setLogicFlagsInt(r);   r }

  def andByte(s: Int, d: Int): Int  = { val r = (s & d) & 0xFF;   setLogicFlagsByte(r);  r }
  def andShort(s: Int, d: Int): Int = { val r = (s & d) & 0xFFFF; setLogicFlagsShort(r); r }
  def andInt(s: Int, d: Int): Int   = { val r = s & d;            setLogicFlagsInt(r);   r }

  def orByte(s: Int, d: Int): Int  = { val r = (s | d) & 0xFF;   setLogicFlagsByte(r);  r }
  def orShort(s: Int, d: Int): Int = { val r = (s | d) & 0xFFFF; setLogicFlagsShort(r); r }
  def orInt(s: Int, d: Int): Int   = { val r = s | d;            setLogicFlagsInt(r);   r }

  def eorByte(s: Int, d: Int): Int  = { val r = (s ^ d) & 0xFF;   setLogicFlagsByte(r);  r }
  def eorShort(s: Int, d: Int): Int = { val r = (s ^ d) & 0xFFFF; setLogicFlagsShort(r); r }
  def eorInt(s: Int, d: Int): Int   = { val r = s ^ d;            setLogicFlagsInt(r);   r }

  /** TST is just "set N/Z, clear V/C" applied to the operand. */
  def tstByte(d: Int): Unit  = setLogicFlagsByte(d & 0xFF)
  def tstShort(d: Int): Unit = setLogicFlagsShort(d & 0xFFFF)
  def tstInt(d: Int): Unit   = setLogicFlagsInt(d)

  // ----------------------------------------------------------------------------
  // Bit shifts — all eight rotate/shift combinations × 3 widths
  //
  // Carry semantics are subtle: for r==0 the shift leaves the operand alone but
  // also clears C (the standard m68k behaviour). X is unchanged when r==0,
  // mirrored from the original implementation.
  // ----------------------------------------------------------------------------

  // ASL (arithmetic shift left) — V set if sign bit ever flipped during the shift
  def aslByte(r: Int, d: Int): Int = {
    val dd = d & 0xFF
    if r == 0 then {
      setC(false); setV(false); setN((dd & 0x80) != 0); setZ(dd == 0)
      return dd
    }
    val res = if r >= 8 then 0 else (dd << r) & 0xFF
    val cb  = if r > 8 then false else ((dd << (r - 1)) & 0x80) != 0
    val signBefore = (dd & 0x80) != 0
    val v = {
      if r >= 8 then dd != 0
      else {
        var changed = false
        var i = 0
        while (i < r && !changed) { if (((dd << i) & 0x80) != 0) != signBefore then changed = true; i += 1 }
        changed || ((res & 0x80) != 0) != signBefore
      }
    }
    writeCCR(c = cb, v = v, z = res == 0, n = (res & 0x80) != 0, x = cb)
    res
  }

  def aslShort(r: Int, d: Int): Int = {
    val dd = d & 0xFFFF
    if r == 0 then { setC(false); setV(false); setN((dd & 0x8000) != 0); setZ(dd == 0); return dd }
    val res = if r >= 16 then 0 else (dd << r) & 0xFFFF
    val cb  = if r > 16 then false else ((dd.toLong << (r - 1)) & 0x8000L) != 0L
    val signBefore = (dd & 0x8000) != 0
    val v = if r >= 16 then dd != 0 else {
      var changed = false
      var i = 0
      while (i < r && !changed) { if (((dd << i) & 0x8000) != 0) != signBefore then changed = true; i += 1 }
      changed || ((res & 0x8000) != 0) != signBefore
    }
    writeCCR(c = cb, v = v, z = res == 0, n = (res & 0x8000) != 0, x = cb)
    res
  }

  def aslInt(r: Int, d: Int): Int = {
    if r == 0 then { setC(false); setV(false); setN(d < 0); setZ(d == 0); return d }
    val res = if r >= 32 then 0 else d << r
    val cb  = if r > 32 then false else (((d.toLong & 0xFFFFFFFFL) << (r - 1)) & 0x80000000L) != 0
    val signBefore = d < 0
    val v = if r >= 32 then d != 0 else {
      var changed = false
      var i = 0
      while (i < r && !changed) { if (((d << i) & 0x80000000) != 0) != signBefore then changed = true; i += 1 }
      changed || (res < 0) != signBefore
    }
    writeCCR(c = cb, v = v, z = res == 0, n = res < 0, x = cb)
    res
  }

  // ASR (arithmetic shift right) — sign-extends; V always cleared
  def asrByte(r: Int, d: Int): Int = {
    val dd = (d << 24) >> 24 // sign-extend
    if r == 0 then { setC(false); setV(false); setN(dd < 0); setZ(dd == 0); return dd & 0xFF }
    val res = (if r >= 8 then dd >> 7 else dd >> r) & 0xFF
    val cb  = if r > 8 then dd < 0 else ((dd >> (r - 1)) & 1) != 0
    writeCCR(c = cb, v = false, z = res == 0, n = (res & 0x80) != 0, x = cb)
    res
  }

  def asrShort(r: Int, d: Int): Int = {
    val dd = (d << 16) >> 16
    if r == 0 then { setC(false); setV(false); setN(dd < 0); setZ(dd == 0); return dd & 0xFFFF }
    val res = (if r >= 16 then dd >> 15 else dd >> r) & 0xFFFF
    val cb  = if r > 16 then dd < 0 else ((dd >> (r - 1)) & 1) != 0
    writeCCR(c = cb, v = false, z = res == 0, n = (res & 0x8000) != 0, x = cb)
    res
  }

  def asrInt(r: Int, d: Int): Int = {
    if r == 0 then { setC(false); setV(false); setN(d < 0); setZ(d == 0); return d }
    val res = if r >= 32 then d >> 31 else d >> r
    val cb  = if r > 32 then d < 0 else ((d >> (r - 1)) & 1) != 0
    writeCCR(c = cb, v = false, z = res == 0, n = res < 0, x = cb)
    res
  }

  // LSL (logical shift left) — same as ASL for the result, but V always cleared
  def lslByte(r: Int, d: Int): Int = {
    val dd = d & 0xFF
    if r == 0 then { setC(false); setV(false); setN((dd & 0x80) != 0); setZ(dd == 0); return dd }
    val res = if r >= 8 then 0 else (dd << r) & 0xFF
    val cb  = if r > 8 then false else ((dd << (r - 1)) & 0x80) != 0
    writeCCR(c = cb, v = false, z = res == 0, n = (res & 0x80) != 0, x = cb)
    res
  }

  def lslShort(r: Int, d: Int): Int = {
    val dd = d & 0xFFFF
    if r == 0 then { setC(false); setV(false); setN((dd & 0x8000) != 0); setZ(dd == 0); return dd }
    val res = if r >= 16 then 0 else (dd << r) & 0xFFFF
    val cb  = if r > 16 then false else ((dd << (r - 1)) & 0x8000) != 0
    writeCCR(c = cb, v = false, z = res == 0, n = (res & 0x8000) != 0, x = cb)
    res
  }

  def lslInt(r: Int, d: Int): Int = {
    if r == 0 then { setC(false); setV(false); setN(d < 0); setZ(d == 0); return d }
    val res = if r >= 32 then 0 else d << r
    val cb  = if r > 32 then false else (((d.toLong & 0xFFFFFFFFL) << (r - 1)) & 0x80000000L) != 0
    writeCCR(c = cb, v = false, z = res == 0, n = res < 0, x = cb)
    res
  }

  // LSR (logical shift right) — zero-fills; V always cleared
  def lsrByte(r: Int, d: Int): Int = {
    val dd = d & 0xFF
    if r == 0 then { setC(false); setV(false); setN((dd & 0x80) != 0); setZ(dd == 0); return dd }
    val res = if r >= 8 then 0 else (dd >>> r) & 0xFF
    val cb  = if r > 8 then false else ((dd >>> (r - 1)) & 1) != 0
    writeCCR(c = cb, v = false, z = res == 0, n = (res & 0x80) != 0, x = cb)
    res
  }

  def lsrShort(r: Int, d: Int): Int = {
    val dd = d & 0xFFFF
    if r == 0 then { setC(false); setV(false); setN((dd & 0x8000) != 0); setZ(dd == 0); return dd }
    val res = if r >= 16 then 0 else (dd >>> r) & 0xFFFF
    val cb  = if r > 16 then false else ((dd >>> (r - 1)) & 1) != 0
    writeCCR(c = cb, v = false, z = res == 0, n = (res & 0x8000) != 0, x = cb)
    res
  }

  def lsrInt(r: Int, d: Int): Int = {
    if r == 0 then { setC(false); setV(false); setN(d < 0); setZ(d == 0); return d }
    val res = if r >= 32 then 0 else d >>> r
    val cb  = if r > 32 then false else ((d >>> (r - 1)) & 1) != 0
    writeCCR(c = cb, v = false, z = res == 0, n = res < 0, x = cb)
    res
  }

  // ROL (rotate left, no extend) — V cleared, X unchanged
  def rolByte(r: Int, d: Int): Int = {
    val dd = d & 0xFF
    if r == 0 then {
      setC(false); setV(false); setN((dd & 0x80) != 0); setZ(dd == 0); return dd
    }
    val rr = r & 7
    val res = if rr == 0 then dd else ((dd << rr) | (dd >>> (8 - rr))) & 0xFF
    val cb  = (res & 1) != 0
    ccr = (ccr & CCR.X) |
      (if cb then CCR.C else 0) |
      (if res == 0 then CCR.Z else 0) |
      (if (res & 0x80) != 0 then CCR.N else 0)
    res
  }

  def rolShort(r: Int, d: Int): Int = {
    val dd = d & 0xFFFF
    if r == 0 then { setC(false); setV(false); setN((dd & 0x8000) != 0); setZ(dd == 0); return dd }
    val rr = r & 15
    val res = if rr == 0 then dd else ((dd << rr) | (dd >>> (16 - rr))) & 0xFFFF
    val cb  = (res & 1) != 0
    ccr = (ccr & CCR.X) |
      (if cb then CCR.C else 0) |
      (if res == 0 then CCR.Z else 0) |
      (if (res & 0x8000) != 0 then CCR.N else 0)
    res
  }

  def rolInt(r: Int, d: Int): Int = {
    if r == 0 then { setC(false); setV(false); setN(d < 0); setZ(d == 0); return d }
    val rr = r & 31
    val res = if rr == 0 then d else (d << rr) | (d >>> (32 - rr))
    val cb  = (res & 1) != 0
    ccr = (ccr & CCR.X) |
      (if cb then CCR.C else 0) |
      (if res == 0 then CCR.Z else 0) |
      (if res < 0 then CCR.N else 0)
    res
  }

  // ROR (rotate right, no extend) — V cleared, X unchanged
  def rorByte(r: Int, d: Int): Int = {
    val dd = d & 0xFF
    if r == 0 then { setC(false); setV(false); setN((dd & 0x80) != 0); setZ(dd == 0); return dd }
    val rr = r & 7
    val res = if rr == 0 then dd else ((dd >>> rr) | (dd << (8 - rr))) & 0xFF
    val cb  = (res & 0x80) != 0
    ccr = (ccr & CCR.X) |
      (if cb then CCR.C else 0) |
      (if res == 0 then CCR.Z else 0) |
      (if cb then CCR.N else 0)
    res
  }

  def rorShort(r: Int, d: Int): Int = {
    val dd = d & 0xFFFF
    if r == 0 then { setC(false); setV(false); setN((dd & 0x8000) != 0); setZ(dd == 0); return dd }
    val rr = r & 15
    val res = if rr == 0 then dd else ((dd >>> rr) | (dd << (16 - rr))) & 0xFFFF
    val cb  = (res & 0x8000) != 0
    ccr = (ccr & CCR.X) |
      (if cb then CCR.C else 0) |
      (if res == 0 then CCR.Z else 0) |
      (if cb then CCR.N else 0)
    res
  }

  def rorInt(r: Int, d: Int): Int = {
    if r == 0 then { setC(false); setV(false); setN(d < 0); setZ(d == 0); return d }
    val rr = r & 31
    val res = if rr == 0 then d else (d >>> rr) | (d << (32 - rr))
    val cb  = res < 0
    ccr = (ccr & CCR.X) |
      (if cb then CCR.C else 0) |
      (if res == 0 then CCR.Z else 0) |
      (if cb then CCR.N else 0)
    res
  }

  // ROXL (rotate left through extend) — X is part of the rotation; V cleared
  def roxlByte(r: Int, d: Int): Int = {
    val dd = d & 0xFF
    val rr = r % 9
    if rr == 0 then {
      // r was 0, 9, 18 ... — value unchanged, but C copies X
      val cb = X
      writeCCR(c = cb, v = false, z = dd == 0, n = (dd & 0x80) != 0, x = X)
      return dd
    }
    val xb  = if X then 1 else 0
    val res = ((dd << rr) | (xb << (rr - 1)) | (dd >>> (9 - rr))) & 0xFF
    val cb  = ((dd >>> (8 - rr)) & 1) != 0
    writeCCR(c = cb, v = false, z = res == 0, n = (res & 0x80) != 0, x = cb)
    res
  }

  def roxlShort(r: Int, d: Int): Int = {
    val dd = d & 0xFFFF
    val rr = r % 17
    if rr == 0 then {
      val cb = X
      writeCCR(c = cb, v = false, z = dd == 0, n = (dd & 0x8000) != 0, x = X)
      return dd
    }
    val xb  = if X then 1 else 0
    val res = ((dd << rr) | (xb << (rr - 1)) | (dd >>> (17 - rr))) & 0xFFFF
    val cb  = ((dd >>> (16 - rr)) & 1) != 0
    writeCCR(c = cb, v = false, z = res == 0, n = (res & 0x8000) != 0, x = cb)
    res
  }

  def roxlInt(r: Int, d: Int): Int = {
    val rr = r % 33
    if rr == 0 then {
      val cb = X
      writeCCR(c = cb, v = false, z = d == 0, n = d < 0, x = X)
      return d
    }
    val xb  = if X then 1L else 0L
    val u   = d.toLong & 0xFFFFFFFFL
    val res = (((u << rr) | (xb << (rr - 1)) | (u >>> (33 - rr))) & 0xFFFFFFFFL).toInt
    val cb  = ((u >>> (32 - rr)) & 1L) != 0
    writeCCR(c = cb, v = false, z = res == 0, n = res < 0, x = cb)
    res
  }

  // ROXR (rotate right through extend)
  def roxrByte(r: Int, d: Int): Int = {
    val dd = d & 0xFF
    val rr = r % 9
    if rr == 0 then {
      val cb = X
      writeCCR(c = cb, v = false, z = dd == 0, n = (dd & 0x80) != 0, x = X)
      return dd
    }
    val xb  = if X then 1 else 0
    val res = ((dd >>> rr) | (xb << (8 - rr)) | (dd << (9 - rr))) & 0xFF
    val cb  = ((dd >>> (rr - 1)) & 1) != 0
    writeCCR(c = cb, v = false, z = res == 0, n = (res & 0x80) != 0, x = cb)
    res
  }

  def roxrShort(r: Int, d: Int): Int = {
    val dd = d & 0xFFFF
    val rr = r % 17
    if rr == 0 then {
      val cb = X
      writeCCR(c = cb, v = false, z = dd == 0, n = (dd & 0x8000) != 0, x = X)
      return dd
    }
    val xb  = if X then 1 else 0
    val res = ((dd >>> rr) | (xb << (16 - rr)) | (dd << (17 - rr))) & 0xFFFF
    val cb  = ((dd >>> (rr - 1)) & 1) != 0
    writeCCR(c = cb, v = false, z = res == 0, n = (res & 0x8000) != 0, x = cb)
    res
  }

  def roxrInt(r: Int, d: Int): Int = {
    val rr = r % 33
    if rr == 0 then {
      val cb = X
      writeCCR(c = cb, v = false, z = d == 0, n = d < 0, x = X)
      return d
    }
    val xb  = if X then 1L else 0L
    val u   = d.toLong & 0xFFFFFFFFL
    val res = (((u >>> rr) | (xb << (32 - rr)) | (u << (33 - rr))) & 0xFFFFFFFFL).toInt
    val cb  = ((u >>> (rr - 1)) & 1L) != 0
    writeCCR(c = cb, v = false, z = res == 0, n = res < 0, x = cb)
    res
  }

  // ----------------------------------------------------------------------------
  // BCD (always byte-wide)
  // ----------------------------------------------------------------------------

  def abcd(s: Int, d: Int): Int = {
    val sum = fromBCD(s) + fromBCD(d) + (if X then 1 else 0)
    val (r, c) = if sum > 99 then (sum - 100, true) else (sum, false)
    setC(c); setX(c)
    if r != 0 then setZ(false) // BCD ops only clear Z on non-zero result; Z otherwise sticky
    toBCD(r)
  }

  def sbcd(s: Int, d: Int): Int = {
    val diff = fromBCD(d) - fromBCD(s) - (if X then 1 else 0)
    val (r, c) = if diff < 0 then (100 + diff, true) else (diff, false)
    setC(c); setX(c)
    if r != 0 then setZ(false)
    toBCD(r)
  }

  def nbcd(d: Int): Int = {
    val diff = -fromBCD(d) - (if X then 1 else 0)
    val (r, c) = if diff < 0 then (100 + diff, true) else (diff, false)
    setC(c); setX(c)
    if r != 0 then setZ(false)
    toBCD(r)
  }

  // ----------------------------------------------------------------------------
  // Conditional evaluation (Bcc / DBcc / Scc)
  // ----------------------------------------------------------------------------

  def testcc(cond: Int): Boolean = cond match {
    case Conditional.True          => true
    case Conditional.False         => false
    case Conditional.High          => !C && !Z
    case Conditional.LowSame       => C || Z
    case Conditional.CarryClear    => !C
    case Conditional.CarrySet      => C
    case Conditional.NotEqual      => !Z
    case Conditional.Equal         => Z
    case Conditional.OverflowClear => !V
    case Conditional.OverflowSet   => V
    case Conditional.Plus          => !N
    case Conditional.Minus         => N
    case Conditional.GreaterEqual  => N == V
    case Conditional.LessThan      => N != V
    case Conditional.GreaterThan   => (N == V) && !Z
    case Conditional.LessEqual     => (N != V) || Z
  }

  // ----------------------------------------------------------------------------
  // Exception entry — pushes SR + PC, switches to supervisor mode, vectors PC
  // ----------------------------------------------------------------------------

  def exception(level: Int, vector: Int): Unit = {
    // m68k entry sequence: latch SR, switch to supervisor + clear T, then push the latched SR and the PC onto
    // the SUPERVISOR stack. Doing the mode switch before the pushes is what guarantees both words land on SSP
    // even if the exception was taken from user mode.
    val savedSR = fromSR
    if level > 0 then sr = (sr & ~SRBit.I) | (level << SRBit.I_shift)
    sr = (sr | SRBit.S) & ~SRBit.T
    pushInt(PC)
    pushShort(savedSR)
    jumpTo(memory.readInt(vector))
  }

  // ----------------------------------------------------------------------------
  // Hooks for instruction handlers — overridable by Emulator/host
  // ----------------------------------------------------------------------------

  def breakpoint(bkpt: Int): Boolean = false
  def illegal: Boolean               = false
  def trap(vector: Int): Boolean     = false
  def lineA: Boolean                 = false
  def lineF: Boolean                 = false

  def supervisor: Boolean =
    if !supervisorMode then { exception(-1, VectorTable.privilegeViolation); false }
    else true

  // ----------------------------------------------------------------------------
  // Execution loop
  // ----------------------------------------------------------------------------

  /** Fetch one opcode, dispatch through the opcode table, run the instruction. */
  def step(): Unit = {
    fetch()
    OpcodeTable(instruction).apply(this)
    if trace then {
      registers(traceOut)
      disassemble(traceOut)
    }
  }

  /** Run until `step()` is asked to stop (via `cpu.stop()`), `cycles` instructions have executed, or PC lands on a
    * breakpoint. Single-shot breakpoints are cleared the first time they fire so a continued `run()` doesn't trip on
    * them again — that's how `stepOver` works.
    */
  def run(cycles: Long = Long.MaxValue): Long = {
    running = true
    var n    = 0L
    while running && !stopped && n < cycles do {
      breakpointMap.get(PC) match {
        case Some(singleShot) =>
          if singleShot then breakpointMap.remove(PC)
          running = false
        case None =>
          step()
          n += 1
      }
    }
    n
  }

  // ----------------------------------------------------------------------------
  // Pretty-printing for the REPL / debugger
  // ----------------------------------------------------------------------------

  /** Print all 8 D, all 8 A (with USP/SSP shown depending on supervisor mode), PC, SR, and CCR. */
  def registers(out: PrintStream): Unit = {
    var i = 0
    while i < 8 do { out.print(s"D$i=${hexInt(D(i))} "); i += 1 }
    out.println()
    i = 0
    while i < 7 do { out.print(s"A$i=${hexInt(A(i))} "); i += 1 }
    out.print(s"A7=${hexInt(if supervisorMode then SSP else USP)} ")
    out.println()
    out.print(s"PC=${hexAddress(PC)}  SR=${hexShort(fromSR)}  ")
    out.print((if X then "X" else "-") + (if N then "N" else "-") + (if Z then "Z" else "-"))
    out.print((if V then "V" else "-") + (if C then "C" else "-"))
    out.println(if supervisorMode then "  [supervisor]" else "  [user]")
  }

  /** Disassemble the instruction at the current PC and advance PC past it. Returns the byte count consumed (so the
    * caller can step a window at once). The disassembly text is a placeholder mnemonic — the existing Instruction
    * subclasses only return the operator, not full operand text.
    */
  def disassemble(out: PrintStream): Int = {
    val pc0 = PC
    fetch()
    val text = OpcodeTable(instruction).disassemble(this)
    val sym  = reverseSymbols.get(pc0).fold("")(s => s"  <$s>")
    out.println(s"${hexAddress(pc0)}  ${hexShort(instruction)}  $text$sym")
    PC - pc0
  }
}
