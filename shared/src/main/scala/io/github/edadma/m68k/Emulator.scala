package io.github.edadma.m68k

/** Convenience wrapper that pairs a [[CPUWithServices]] with a default 2 MiB RAM map and an SREC loader.
  *
  * Layout:
  *   - 0x0000..0xFFFF — code/data, populated by the SREC loader as ROM segments (the legacy newlib/uclibc m68k
  *     toolchains link here and put the reset vectors at the start of the image)
  *   - 0x10000..0x20FFFF — 2 MiB RAM for stack/heap (legacy convention; SREC images expect SSP somewhere in here)
  *
  * Trimmed compared to the legacy emulator: no symbol-map reader, no debug-info reader, no breakpoint registry, no
  * device-installer registry, no disassemble/dump helpers. Wire those back in if/when needed.
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

  /** Load an SREC image (the parameter is the file's text content, not a path). The S9 start address is returned. */
  def load(srecText: String): Int = {
    if cpu.isRunning then sys.error("can't load while running")
    mem.removeROM()
    mem.reset()
    val start = SREC.read(mem, srecText)
    cpu.reset()
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
    cpu.reset()
  }

  def addDevice(d: Device): Unit = {
    mem.add(d)
    d.connectTo(cpu)
  }

  def run(cycles: Long = Long.MaxValue): Long = {
    val n = cpu.run(cycles)
    cpu.resetSignal()
    n
  }

  def step(): Unit = cpu.step()
  def reset(): Unit = cpu.reset()
  def stop(): Unit = cpu.stop()
}
