package io.github.edadma.m68k

import java.io.{OutputStream, PrintStream}

import scala.collection.mutable

/** Convenience wrapper that pairs a [[CPUWithServices]] with a default 2 MiB RAM map and an SREC loader.
  *
  * Layout:
  *   - 0x0000..0xFFFF — code/data, populated by the SREC loader as ROM segments (the legacy newlib/uclibc m68k
  *     toolchains link here and put the reset vectors at the start of the image)
  *   - 0x10000..0x20FFFF — 2 MiB RAM for stack/heap (legacy convention; SREC images expect SSP somewhere in here)
  *
  * Beyond the bare load/run hooks, this class carries the REPL-targeted surface (dump, disassemble window,
  * symbols, breakpoint listing, target-address resolver, step-over). The REPL itself lives in the JVM `Main`.
  */
class Emulator {

  val mem: Memory = new Memory {
    def init(): Unit = {
      removeDevices()
      regions.clear()
      // Placeholder ROM covering the vector table, so the CPU's constructor-time reset() finds zeroed vectors. The
      // SREC loader removes this and substitutes its own ROM segments at the same addresses before reset() runs again.
      add(new ROM("program", 0, 0xFFFF))
      add(new RAM("ram", 0x10000, 0x10000 + 2 * 1024 * 1024 - 1))
    }
  }

  val cpu: CPUWithServices = new CPUWithServices(mem)

  /** User-defined symbol table — populated by REPL `symbols` command and used by `target` to resolve labels. */
  val symbols: mutable.HashMap[String, Int] = mutable.HashMap.empty

  /** Cursors for "where the dump/disassemble window left off." */
  private[m68k] var dumpCur: Int = 0
  private[m68k] var disCur: Int  = 0

  // ----- Loading -------------------------------------------------------------

  /** Load an SREC image (the parameter is the file's text content, not a path). The S9 start address is returned. */
  def load(srecText: String): Int = {
    if cpu.isRunning then sys.error("can't load while running")
    mem.removeROM()
    mem.reset()
    val start = SREC.read(mem, srecText)
    cpu.clearBreakpoints()
    cpu.reset()
    disCur = cpu.PC
    start
  }

  /** Load a flat binary blob at `loadAddr`.
    *
    * If `loadAddr > 0` we synthesise reset vectors at addr 0 (SSP, then PC=loadAddr) — useful when the source is a
    * raw `.text` extract that knows nothing about vectors. If `loadAddr == 0` the blob is loaded verbatim, on the
    * assumption that the linker put a `.vectors` section at the start of the image (which is what `m68k.ld` does).
    */
  def loadBinary(bytes: Array[Byte], loadAddr: Int = 0x100, ssp: Int = 0x11000): Unit = {
    if cpu.isRunning then sys.error("can't load while running")
    mem.removeROM()
    mem.reset()
    if loadAddr == 0 then {
      mem.add(ROM("image", 0, bytes.toIndexedSeq))
    } else {
      val vectors = new Array[Byte](8)
      vectors(0) = ((ssp >> 24) & 0xFF).toByte
      vectors(1) = ((ssp >> 16) & 0xFF).toByte
      vectors(2) = ((ssp >> 8) & 0xFF).toByte
      vectors(3) = (ssp & 0xFF).toByte
      vectors(4) = ((loadAddr >> 24) & 0xFF).toByte
      vectors(5) = ((loadAddr >> 16) & 0xFF).toByte
      vectors(6) = ((loadAddr >> 8) & 0xFF).toByte
      vectors(7) = (loadAddr & 0xFF).toByte
      mem.add(ROM("vectors", 0, vectors.toIndexedSeq))
      mem.add(ROM("text", loadAddr, bytes.toIndexedSeq))
    }
    cpu.clearBreakpoints()
    cpu.reset()
    disCur = cpu.PC
  }

  def addDevice(d: Device): Unit = {
    mem.add(d)
    d.connectTo(cpu)
  }

  // ----- Run / step ----------------------------------------------------------

  def run(cycles: Long = Long.MaxValue): Long = {
    val n = cpu.run(cycles)
    cpu.resetSignal()
    disCur = cpu.PC
    n
  }

  def step(): Unit = {
    cpu.step()
    disCur = cpu.PC
  }

  /** Step over the current instruction: if it's a JSR/BSR (anything that pushes a return address), set a single-shot
    * breakpoint at PC + length-of-current-instruction and run; otherwise just step. The simple heuristic here is
    * "always set the breakpoint" — non-call instructions hit it on the next step anyway.
    */
  def stepOver(): Unit = {
    val savedPC = cpu.PC
    val savedIR = cpu.instruction
    // Probe instruction length by disassembling against a discard stream, then restore PC.
    val probe   = new PrintStream(new OutputStream { def write(b: Int): Unit = () })
    val len     = cpu.disassemble(probe)
    cpu.PC          = savedPC
    cpu.instruction = savedIR
    cpu.setSingleShotBreakpoint(savedPC + len)
    run()
  }

  def reset(): Unit = {
    cpu.reset()
    cpu.clearBreakpoints()
    disCur = cpu.PC
  }

  def stop(): Unit = cpu.stop()

  // ----- Symbol / target resolution -----------------------------------------

  /** Resolve a textual address reference into an Int — either a hex literal (no `0x` prefix; trailing colon ignored)
    * or a symbol name from `symbols`.
    */
  def target(ref: String): Int = {
    val r = if ref.endsWith(":") then ref.dropRight(1) else ref
    if isHex(r) then hex(r)
    else
      symbols.get(r) match {
        case Some(addr) => addr
        case None       => sys.error(s"unknown label: $r")
      }
  }

  // ----- Inspectors ----------------------------------------------------------

  def breakpoints(out: PrintStream): Unit = {
    if cpu.breakpoints.isEmpty then out.println("(none)")
    else
      cpu.breakpoints.foreach { b =>
        val sym = cpu.reverseSymbols.get(b).fold("")(s => s"  <$s>")
        out.println(s"${hexAddress(b)}$sym")
      }
  }

  /** Hex+ASCII memory dump. `start = -1` continues from the last dump cursor. */
  def dump(start: Int, lines: Int, out: PrintStream): Unit = {
    val addr0 = if start == -1 then dumpCur - dumpCur % 16 else start - start % 16

    def read(a: Int): Option[Int] =
      if mem.addressable(a) && mem.memory(a) then Some(mem.readByte(a)) else None

    var line = addr0
    var n    = 0
    while n < lines && line < ADDRESS_RANGE do {
      out.print(f"${line & 0xFFFFFF}%06X  ")
      var i = 0
      while i < 16 do {
        if i == 8 then out.print(' ')
        read(line + i) match {
          case Some(b) => out.print(f"$b%02X ")
          case None    => out.print("-- ")
        }
        i += 1
      }
      out.print(' ')
      i = 0
      while i < 16 do {
        val c = read(line + i).getOrElse(0)
        out.print(if c >= 0x20 && c < 0x7F then c.toChar else '.')
        i += 1
      }
      out.println()
      line += 16
      n += 1
    }
    dumpCur = addr0 + 16 * lines
  }

  /** Multi-line disassembly. `start = -1` continues from the last disassemble cursor. PC is preserved. */
  def disassemble(start: Int, lines: Int, out: PrintStream): Unit = {
    if start >= 0 then disCur = start
    val savedPC = cpu.PC
    cpu.PC = disCur
    var n = 0
    while n < lines do {
      cpu.disassemble(out)
      n += 1
    }
    disCur = cpu.PC
    cpu.PC = savedPC
  }
}
