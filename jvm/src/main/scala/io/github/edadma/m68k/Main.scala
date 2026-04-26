package io.github.edadma.m68k

import java.io.PrintStream
import java.nio.file.{Files, Paths}

import org.jline.reader.{EndOfFileException, LineReaderBuilder, UserInterruptException}
import org.jline.reader.impl.history.DefaultHistory
import org.jline.terminal.TerminalBuilder

/** JVM entry point. Default behavior is an interactive REPL (jline 3 line editor with history). The previous flat
  * "load + run + exit" CLI is still available via flags.
  *
  * Usage:
  *   m68k                                          → REPL with empty memory map
  *   m68k <file.srec>                              → load SREC into REPL, drop to prompt
  *   m68k <file.srec> --run                        → load + run + exit
  *   m68k --bin <file.bin> [--load-at 0x100]       → load flat binary into REPL
  *   m68k --bin <file.bin> --load-at 0 --run       → load + run + exit
  */
object Main {

  def main(args: Array[String]): Unit = {
    var srec: Option[String]    = None
    var bin: Option[String]     = None
    var loadAt                  = 0x100
    var ssp                     = 0x11000
    var cycles                  = Long.MaxValue
    var runAndExit              = false

    val it = args.iterator
    while it.hasNext do {
      it.next() match {
        case "--help" | "-h" => println(USAGE); return
        case "--bin"     => bin = Some(it.next())
        case "--load-at" => loadAt = decode(it.next())
        case "--ssp"     => ssp = decode(it.next())
        case "--cycles"  => cycles = it.next().toLong
        case "--run"     => runAndExit = true
        case other if !other.startsWith("--") =>
          srec = Some(other)
        case other => sys.error(s"unknown option: $other")
      }
    }

    val emu = new Emulator
    (bin, srec) match {
      case (Some(_), Some(_)) => sys.error("specify either --bin <file> or <file.srec>, not both")
      case (Some(b), None) => emu.loadBinary(Files.readAllBytes(Paths.get(b)), loadAt, ssp)
      case (None, Some(s)) => emu.load(new String(Files.readAllBytes(Paths.get(s))))
      case (None, None)    => () // empty REPL
    }

    if runAndExit then emu.run(cycles)
    else repl(emu)
  }

  private def decode(s: String): Int =
    if s.startsWith("0x") || s.startsWith("0X") then Integer.parseInt(s.drop(2), 16)
    else s.toInt

  // ----- REPL ----------------------------------------------------------------

  private def repl(emu: Emulator): Unit = {
    val terminal = TerminalBuilder.builder().system(true).build()
    val history  = new DefaultHistory()
    val historyFile = Paths.get(System.getProperty("user.home"), ".m68k-repl-history")
    val reader = LineReaderBuilder.builder()
      .terminal(terminal)
      .history(history)
      .variable(org.jline.reader.LineReader.HISTORY_FILE, historyFile)
      .build()

    val out = new PrintStream(terminal.output(), true)

    out.println(s"m68k emulator — type 'help' for commands, 'quit' to exit")

    var done = false
    while !done do {
      val line =
        try reader.readLine("m68k> ")
        catch {
          case _: UserInterruptException => "" // ^C — clear line
          case _: EndOfFileException     => null // ^D — exit
        }
      if line == null then done = true
      else if line.trim.nonEmpty then {
        try done = interp(emu, out, line)
        catch { case e: Exception => out.println(s"error: ${e.getMessage}") }
      }
    }
  }

  /** Returns `true` to exit the REPL. */
  private def interp(emu: Emulator, out: PrintStream, command: String): Boolean = {
    val parts = command.trim.split("\\s+").toList

    val aReg = "[aA]([0-7])".r
    val dReg = "[dD]([0-7])".r

    def regs(): Unit = {
      emu.cpu.registers(out)
      // Show next instruction so user can trace.
      val pc = emu.cpu.PC
      emu.cpu.disassemble(out)
      emu.cpu.PC = pc
    }

    parts match {
      case "help" :: _ | "h" :: _ =>
        out.println(HELP)

      case ("quit" | "q") :: _ =>
        emu.stop()
        return true

      case ("registers" | "r") :: Nil =>
        regs()

      case ("registers" | "r") :: reg :: value :: _ =>
        val n = emu.target(value)
        reg.toLowerCase match {
          case aReg(r) => emu.cpu.writeA(r.toInt, n)
          case dReg(r) => emu.cpu.D(r.toInt) = n
          case "sr"    => emu.cpu.toSR(n)
          case "ccr"   => emu.cpu.toCCR(n)
          case "pc"    => emu.cpu.jumpTo(n)
          case _       => out.println(s"unknown register: $reg")
        }
        regs()

      case ("memory" | "m") :: Nil =>
        out.println(emu.mem)

      case ("memory" | "m") :: addr :: data =>
        val a = emu.target(addr)
        data.zipWithIndex.foreach { case (d, i) => emu.mem.programByte(a + i, emu.target(d)) }
        emu.dump(a, (data.length + a % 16) / 16 + 1, out)

      case ("dump" | "d") :: Nil =>
        emu.dump(-1, 10, out)
      case ("dump" | "d") :: addr :: _ =>
        emu.dump(emu.target(addr), 10, out)

      case ("disassemble" | "u") :: Nil =>
        emu.disassemble(-1, 15, out)
      case ("disassemble" | "u") :: addr :: _ =>
        emu.disassemble(emu.target(addr), 15, out)

      case ("step" | "s") :: Nil =>
        emu.step(); regs()
      case ("step" | "s") :: addr :: _ =>
        emu.cpu.jumpTo(emu.target(addr)); emu.step(); regs()

      case ("stepover" | "so") :: Nil =>
        emu.stepOver(); regs()

      case ("execute" | "e") :: Nil =>
        emu.run(); regs()
      case ("execute" | "e") :: addr :: rest =>
        if addr.startsWith("/") then {
          emu.cpu.setSingleShotBreakpoint(emu.target(addr.drop(1)))
          emu.run(); regs()
        } else {
          emu.cpu.jumpTo(emu.target(addr))
          rest.headOption.foreach { stop =>
            emu.cpu.setSingleShotBreakpoint(emu.target(if stop.startsWith("/") then stop.drop(1) else stop))
          }
          emu.run(); regs()
        }

      case ("reset" | "re") :: _ =>
        emu.reset(); regs()

      case ("stop" | "st") :: _ =>
        emu.stop(); regs()

      case ("breakpoint" | "b") :: Nil =>
        emu.breakpoints(out)
      case ("breakpoint" | "b") :: "-" :: Nil =>
        emu.cpu.clearBreakpoints()
      case ("breakpoint" | "b") :: bp :: _ if bp.startsWith("-") =>
        emu.cpu.clearBreakpoint(emu.target(bp.drop(1)))
        emu.breakpoints(out)
      case ("breakpoint" | "b") :: bp :: _ =>
        emu.cpu.setBreakpoint(emu.target(bp))
        emu.breakpoints(out)

      case ("trace" | "t") :: "on" :: _ =>
        emu.cpu.trace = true; emu.cpu.traceOut = out
      case ("trace" | "t") :: "off" :: _ =>
        emu.cpu.trace = false; emu.cpu.traceOut = Console.out

      case ("symbols" | "sy") :: Nil =>
        if emu.symbols.isEmpty then out.println("(no symbols)")
        else {
          out.println("name             value")
          out.println("----             -----")
          emu.symbols.toList.sortBy(_._1).foreach { case (k, v) => out.println(f"$k%-16s ${v}%06x") }
        }
      case ("symbols" | "sy") :: name :: value :: _ =>
        emu.symbols.update(name, emu.target(value))

      case ("load" | "l") :: file :: _ =>
        if file.endsWith(".bin") then emu.loadBinary(Files.readAllBytes(Paths.get(file)), 0)
        else emu.load(new String(Files.readAllBytes(Paths.get(file))))
        regs()

      case ("clear" | "c") :: Nil =>
        emu.mem.clearRAM()
      case ("clear" | "c") :: a1 :: a2 :: _ =>
        var i = emu.target(a1); val end = emu.target(a2)
        while i < end do { emu.mem.programByte(i, 0); i += 1 }

      case ("drop" | "dr") :: region :: _ =>
        emu.mem.remove(region); out.println(emu.mem)

      case _ =>
        out.println(s"unknown command: ${parts.head} (try 'help')")
    }
    false
  }

  private val USAGE =
    """m68k — Motorola 68000 emulator + REPL
      |
      |Usage:
      |  m68k                                          interactive REPL with empty memory
      |  m68k <file.srec>                              load SREC, drop to REPL
      |  m68k <file.srec> --run                        load + run + exit (non-interactive)
      |  m68k --bin <file.bin> [--load-at 0x100]       load flat binary, drop to REPL
      |  m68k --bin <file.bin> --load-at 0 --run       load + run + exit
      |
      |Common flags:
      |  --cycles N      cap execution at N instructions (--run mode)
      |  --ssp ADDR      override synthesised supervisor stack pointer (--bin only)
      |  --help          this message
      |
      |Addresses accept either decimal or 0x-prefixed hex.
      |""".stripMargin

  private val HELP =
    """commands:
      |  registers (r) [reg val]      print state, or set <reg> to <val>
      |  memory    (m) [addr [data*]] print map, or write bytes at <addr>
      |  dump      (d) [addr]         hex+ASCII dump (continues from previous if no addr)
      |  disassemble (u) [addr]       disassemble window (continues if no addr)
      |  step      (s) [addr]         execute one instruction
      |  stepover  (so)               step over a JSR/BSR (single-shot bp at PC + length)
      |  execute   (e) [addr [/stop]] run from PC (or addr); /stop sets a single-shot bp
      |  reset     (re)               reset CPU, reload vectors
      |  stop      (st)               request stop
      |  breakpoint (b) [-|-addr|addr] list / clear all / clear / set
      |  trace     (t) on|off         per-instruction register + disasm output
      |  symbols   (sy) [name val]    list, or define a symbol
      |  load      (l) <file>         load .srec or .bin
      |  clear     (c) [a1 a2]        zero RAM, or zero range [a1, a2)
      |  drop      (dr) <region>      remove a memory region by name
      |  quit      (q)
      |
      |addresses are hex (no prefix) or symbol names; trailing colon ignored.
      |""".stripMargin
}
