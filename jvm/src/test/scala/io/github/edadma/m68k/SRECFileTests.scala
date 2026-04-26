package io.github.edadma.m68k

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Paths}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** JVM-only end-to-end checks against the GCC-built newlib binaries that live in `tests/`. These confirm the SREC
  * loader, opcode dispatch, addressing modes, and TRAP #15 services agree with what a real m68k toolchain emits.
  *
  * Skipped silently when the test file is missing — keeps the suite green for fresh checkouts that haven't built
  * the toolchain yet.
  */
class SRECFileTests extends AnyFreeSpec with Matchers {

  private def captureStdout(body: => Unit): String = {
    val baos = new ByteArrayOutputStream
    Console.withOut(new PrintStream(baos)) { body }
    baos.toString("UTF-8")
  }

  private def runFile(name: String, cycles: Long = 10_000_000L): Option[String] = {
    val p = Paths.get("tests", name)
    if !Files.exists(p) then None
    else {
      val text = new String(Files.readAllBytes(p))
      val emu  = new Emulator
      Some(captureStdout {
        emu.load(text)
        emu.run(cycles)
      })
    }
  }

  "tests/hello.srec prints Hello world!" in {
    runFile("hello.srec").foreach(_ should include("Hello world!"))
  }

  "tests/printf.srec prints the expected formatted output" in {
    runFile("printf.srec").foreach { out =>
      out should include("asdf")
      out should include("123")
    }
  }

  "tests/quicksort.srec runs without crashing and prints sorted partitions" in {
    runFile("quicksort.srec").foreach { out =>
      // The C source partitions a small array; the final partition print should contain a sorted prefix.
      out should include("[1, 2, 3,")
    }
  }

  "examples/clang/hello.bin (clang + m68k-elf-ld) prints greeting and counts A..F" in {
    val p = Paths.get("examples/clang/hello.bin")
    if Files.exists(p) then {
      val emu = new Emulator
      val out = captureStdout {
        // hello.bin is linked with m68k.ld so vectors are at addr 0; load verbatim with --load-at 0.
        emu.loadBinary(Files.readAllBytes(p), loadAddr = 0)
        emu.run(cycles = 1000)
      }
      out should include("Hello from clang + m68k-elf-ld!")
      out should include("ABCDEF")
    }
  }
}
