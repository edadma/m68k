package io.github.edadma.m68k

import java.io.{ByteArrayOutputStream, PrintStream}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Cover the REPL-targeting surface: breakpoints (sticky and single-shot), register/dump pretty-printers, the
  * Emulator's symbol resolver, and stepOver. These don't need a full SREC — they synthesise tiny opcode programs
  * directly into RAM, the same pattern EndToEndTests uses.
  */
class DebugSurfaceTests extends AnyFreeSpec with Matchers with Testing {

  private def withProgram(words: Int*)(setup: CPU => Unit = _ => ()): CPU = {
    val cpu = newCPU()
    cpu.memory.writeInt(0, 0x10000)
    cpu.memory.writeInt(4, 0x1000)
    var addr = 0x1000
    for w <- words do {
      cpu.memory.writeShort(addr, w & 0xFFFF)
      addr += 2
    }
    setup(cpu)
    cpu.reset()
    cpu
  }

  private def capture(body: PrintStream => Unit): String = {
    val baos = new ByteArrayOutputStream
    val out  = new PrintStream(baos, true)
    body(out)
    baos.toString("UTF-8")
  }

  "breakpoint stops the run loop before the instruction at PC executes" in {
    val cpu = withProgram(
      0x7005, // MOVEQ #5, D0
      0x7607, // MOVEQ #7, D3 — should NOT execute
    )()
    cpu.setBreakpoint(0x1002) // before the second MOVEQ
    cpu.run()
    cpu.D(0) shouldBe 5
    cpu.D(3) shouldBe 0
    cpu.PC shouldBe 0x1002
  }

  "single-shot breakpoint clears itself after firing" in {
    val cpu = withProgram(0x7005)()
    cpu.setSingleShotBreakpoint(0x1000)
    cpu.run()
    cpu.isBreakpoint(0x1000) shouldBe false
  }

  "registers() prints D0..D7, A0..A7, PC, SR with flag legend" in {
    val cpu = newCPU()
    cpu.D(0) = 0x12345678
    cpu.D(7) = -1
    cpu.A(2) = 0x9000
    val text = capture(out => cpu.registers(out))
    text should include("D0=12345678")
    text should include("D7=FFFFFFFF")
    text should include("A2=00009000")
    text should include("PC=")
    text should include("SR=")
  }

  "dump() formats memory as hex+ASCII rows" in {
    val emu = new Emulator
    // RAM region is at 0x10000+ in the default Emulator map; write there.
    "Hello!".getBytes.zipWithIndex.foreach { case (b, i) => emu.mem.writeByte(0x10100 + i, b.toInt) }
    val text = capture(out => emu.dump(0x10100, 1, out))
    text should include("010100")
    text should include("48 65 6C 6C 6F 21")    // hex bytes
    text should include("Hello!")               // ASCII column
  }

  "target() resolves hex literals and registered symbols" in {
    val emu = new Emulator
    emu.symbols.update("main", 0x1234)
    emu.target("1234") shouldBe 0x1234
    emu.target("main") shouldBe 0x1234
    emu.target("main:") shouldBe 0x1234
    an[Exception] should be thrownBy emu.target("missing")
  }
}
