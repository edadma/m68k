package io.github.edadma.m68k

/** Common test fixture: a freshly-reset CPU with a 4 KiB RAM at 0x0000 (which contains the reset vectors at
  * 0x000/0x004) plus a 60 KiB RAM at 0x1000–0xFFFF for instruction operands and stack space.
  *
  * Tests configure the CPU via the public/package-private API (`writeDInt`, `writeA`, `ccr = ...`, ...) and assert
  * results either by reading registers/memory directly or via the `flags` helper, which formats the CCR as a
  * five-character string in the style the original test corpus used (`"xnzvc"`).
  */
trait Testing {

  def newCPU(): CPU = {
    val mem = new Memory {
      def init(): Unit = {
        // 64 KiB contiguous RAM starting at 0 covers the reset vectors and gives operand
        // space at 0x1000+ and a stack area we can grow downward from 0xFFFF.
        add(new RAM("ram", 0x0000, 0xFFFF))
      }
    }
    new CPU(mem)
  }

  /** Run `setup` against a freshly-reset CPU and return (result, ccr-string). */
  def runOn[A](setup: CPU => A): (A, String) = {
    val cpu = newCPU()
    val r   = setup(cpu)
    (r, flags(cpu))
  }

  def flags(cpu: CPU): String =
    (if cpu.X then "x" else "") +
      (if cpu.N then "n" else "") +
      (if cpu.Z then "z" else "") +
      (if cpu.V then "v" else "") +
      (if cpu.C then "c" else "")

  /** Sign-extend an N-byte value already in the low bits of `v` to a full Int. */
  inline def signByte(v: Int): Int  = (v << 24) >> 24
  inline def signShort(v: Int): Int = (v << 16) >> 16

  /** Mask an N-byte unsigned value to its bit width. */
  inline def unsignByte(v: Int): Int  = v & 0xFF
  inline def unsignShort(v: Int): Int = v & 0xFFFF
}
