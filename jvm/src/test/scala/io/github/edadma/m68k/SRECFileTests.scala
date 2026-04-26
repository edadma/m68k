package io.github.edadma.m68k

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Paths}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** End-to-end checks against the GCC-built newlib binaries that live in `tests/`. Each program runs to halt and the
  * captured stdout is compared *exactly* against the expected string — a much stronger CPU-divergence canary than
  * `should include`, since any subtle ALU/flag bug shows up as a character mismatch instead of getting masked.
  *
  * Skipped silently when a `.srec` is missing (they're gitignored as build artifacts of the m68k-elf toolchain),
  * keeping the suite green for fresh checkouts that haven't built the corpus yet.
  */
class SRECFileTests extends AnyFreeSpec with Matchers {

  private def captureStdout(body: => Unit): String = {
    val baos = new ByteArrayOutputStream
    Console.withOut(new PrintStream(baos)) { body }
    baos.toString("UTF-8")
  }

  /** Run `tests/$name.srec` to halt and return the captured stdout, trimmed of trailing whitespace. None when the
    * file is missing.
    */
  private def runSrec(name: String, cycles: Long = 100_000_000L): Option[String] = {
    val p = Paths.get("tests", s"$name.srec")
    if !Files.exists(p) then None
    else {
      val text = new String(Files.readAllBytes(p))
      val emu  = new Emulator
      val out  = captureStdout {
        emu.load(text)
        emu.run(cycles)
      }
      Some(out.trim)
    }
  }

  /** Helper: assert exact output if the .srec is present, otherwise no-op. */
  private def expect(name: String, expected: String): Unit =
    runSrec(name).foreach(_ shouldBe expected)

  // ---- single-line outputs --------------------------------------------------

  "hello"     in expect("hello",     "Hello world!")
  "printf"    in expect("printf",    "| asdf| |  123| |   7b| |  3.4| 3.400000e+00")
  "pi_approx" in expect("pi_approx", "3.141592653589791339641124")

  // ---- multi-line outputs ---------------------------------------------------

  "strcmp" in expect("strcmp",
    """zero
      |zero
      |negative
      |positive
      |zero
      |positive
      |negative
      |positive
      |negative""".stripMargin)

  "str2int" in expect("str2int",
    """yes
      |yes
      |yes
      |yes
      |yes
      |yes
      |yes
      |yes
      |yes""".stripMargin)

  "int2str" in expect("int2str",
    """zero
      |zero
      |zero
      |zero
      |zero""".stripMargin)

  "int2stru" in expect("int2stru",
    """zero
      |zero
      |zero
      |zero""".stripMargin)

  "int2str64" in expect("int2str64",
    """0
      |123
      |12AB
      |2000000000
      |20000000000
      |-123
      |-12AB
      |-2000000000
      |-20000000000""".stripMargin)

  "int2str64u" in expect("int2str64u",
    """0
      |123
      |12AB
      |2000000000
      |20000000000
      |F000000000000000""".stripMargin)

  "armstrong" in expect("armstrong",
    """0
      |1
      |153
      |370
      |371
      |407""".stripMargin)

  "armstrong64" in expect("armstrong64",
    """0
      |1
      |153
      |370
      |371
      |407""".stripMargin)

  // quicksort family — same input, same expected output across all widths
  private val quicksortExpected =
    """[1, 2, 3, 7, 7, 7, 7, 8, 9, 10]
      |[1, 2, 3, 5, 7, 7, 7, 8, 9, 10]
      |[1, 2, 3, 4, 5, 7, 7, 8, 9, 10]
      |[9, 10]
      |[10]
      |[]""".stripMargin

  "quicksort"   in expect("quicksort",   quicksortExpected)
  "quicksort8"  in expect("quicksort8",  quicksortExpected)
  "quicksort16" in expect("quicksort16", quicksortExpected)
  "quicksort64" in expect("quicksort64", quicksortExpected)

  "bittwiddling" in expect("bittwiddling",
    """5, 0, 5
      |0, 20, 57, 77
      |20, 57
      |0, 1
      |1, 0""".stripMargin)

  "bittwiddling8" in expect("bittwiddling8",
    """5, 0, 5
      |0, 20, 57, 77
      |20, 57
      |0, 1""".stripMargin)

  "bittwiddling16" in expect("bittwiddling16",
    """5, 0, 5
      |0, 20, 57, 77
      |20, 57
      |0, 1""".stripMargin)

  "bittwiddling64" in expect("bittwiddling64",
    """5, 0, 5
      |0, 20, 57, 77
      |20, 57
      |0, 1
      |1, 0""".stripMargin)

  "bittwiddling64u" in expect("bittwiddling64u",
    """0, 20, 57, 77
      |20, 57
      |1, 0""".stripMargin)

  "float64" in expect("float64",
    """true
      |true
      |true
      |true
      |false
      |false
      |true
      |true
      |true
      |3.5
      |-3.5
      |true
      |true
      |true
      |true
      |true
      |true
      |true
      |true
      |true
      |true
      |true
      |true""".stripMargin)

  // ---- the new clang-built binary in examples/ -----------------------------

  "examples/clang/hello.bin (clang + m68k-elf-ld) prints greeting and counts A..F" in {
    val p = Paths.get("examples/clang/hello.bin")
    if Files.exists(p) then {
      val emu = new Emulator
      val out = captureStdout {
        emu.loadBinary(Files.readAllBytes(p), loadAddr = 0)
        emu.run(cycles = 1000)
      }
      out should include("Hello from clang + m68k-elf-ld!")
      out should include("ABCDEF")
    }
  }
}
