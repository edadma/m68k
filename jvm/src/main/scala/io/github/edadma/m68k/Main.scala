package io.github.edadma.m68k

import java.nio.file.{Files, Paths}

object Main {

  def main(args: Array[String]): Unit = {
    var bin: Option[String] = None
    var srec: Option[String] = None
    var loadAt = 0x100
    var ssp    = 0x11000
    var cycles = Long.MaxValue

    val it = args.iterator
    while it.hasNext do {
      it.next() match {
        case "--help" | "-h" => usage(); return
        case "--bin"     => bin = Some(it.next())
        case "--load-at" => loadAt = decode(it.next())
        case "--ssp"     => ssp = decode(it.next())
        case "--cycles"  => cycles = it.next().toLong
        case other if !other.startsWith("--") =>
          srec = Some(other)
        case other => sys.error(s"unknown option: $other")
      }
    }

    val emu = new Emulator
    (bin, srec) match {
      case (Some(_), Some(_)) =>
        sys.error("specify either --bin <file> or <file.srec>, not both")
      case (Some(b), None) =>
        emu.loadBinary(Files.readAllBytes(Paths.get(b)), loadAt, ssp)
      case (None, Some(s)) =>
        emu.load(new String(Files.readAllBytes(Paths.get(s))))
      case (None, None) =>
        usage()
        return
    }
    emu.run(cycles)
  }

  private def decode(s: String): Int =
    if s.startsWith("0x") || s.startsWith("0X") then Integer.parseInt(s.drop(2), 16)
    else s.toInt

  private def usage(): Unit =
    println(
      """m68k — Motorola 68000 emulator
        |
        |Usage:
        |  m68k <file.srec>                          load and run an S-record image
        |  m68k --bin <file.bin> [--load-at 0x100]   load a flat binary at the given address
        |                                            (default 0x100), with synthesised reset
        |                                            vectors (SSP=0x11000, PC=load address)
        |
        |Common flags:
        |  --cycles N      cap execution at N instructions (default: unlimited)
        |  --ssp ADDR      override the synthesised supervisor stack pointer (--bin only)
        |  --help          this message
        |
        |Addresses accept either decimal or 0x-prefixed hex.
        |""".stripMargin,
    )
}
