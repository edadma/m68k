package io.github.edadma.m68k

import scala.collection.mutable.ArrayBuffer

trait Addressable {

  def name: String
  def start: Int
  def size: Int

  def readByte(addr: Int): Int
  def writeByte(addr: Int, value: Int): Unit

  def isRAM: Boolean    = isInstanceOf[RAM]
  def isROM: Boolean    = isInstanceOf[ROM]
  def isDevice: Boolean = isInstanceOf[Device]

  def programByte(addr: Int, value: Int): Unit = writeByte(addr, value)

  def programShort(addr: Int, value: Int): Unit = {
    programByte(addr, (value >> 8) & 0xFF)
    programByte(addr + 1, value & 0xFF)
  }

  def programInt(addr: Int, value: Int): Unit = {
    programShort(addr, (value >> 16) & 0xFFFF)
    programShort(addr + 2, value & 0xFFFF)
  }

  def programLong(addr: Int, value: Long): Unit = {
    programInt(addr, (value >> 32).toInt)
    programInt(addr + 4, value.toInt)
  }

  def readShort(addr: Int): Int =
    ((readByte(addr) & 0xFF) << 8) | (readByte(addr + 1) & 0xFF)

  def writeShort(addr: Int, value: Int): Unit = {
    writeByte(addr, (value >> 8) & 0xFF)
    writeByte(addr + 1, value & 0xFF)
  }

  def readInt(addr: Int): Int =
    (readShort(addr) << 16) | (readShort(addr + 2) & 0xFFFF)

  def writeInt(addr: Int, value: Int): Unit = {
    writeShort(addr, (value >> 16) & 0xFFFF)
    writeShort(addr + 2, value & 0xFFFF)
  }

  def readLong(addr: Int): Long =
    (readInt(addr).toLong << 32) | (readInt(addr + 4) & 0xFFFFFFFFL)

  def writeLong(addr: Int, value: Long): Unit = {
    writeInt(addr, (value >> 32).toInt)
    writeInt(addr + 4, value.toInt)
  }

  def contains(addr: Int): Boolean = addr >= start && addr < start + size
}

class RAM(val name: String, val start: Int, val end: Int) extends Addressable {
  require(start >= 0, s"RAM start must be ≥ 0, got $start")
  require(end >= start, s"RAM end ($end) must be ≥ start ($start)")

  val size = end - start + 1

  protected val mem: Array[Byte] = new Array[Byte](size)

  def clear(): Unit = java.util.Arrays.fill(mem, 0.toByte)

  def readByte(addr: Int): Int               = mem(addr - start) & 0xFF
  def writeByte(addr: Int, value: Int): Unit = mem(addr - start) = value.toByte

  override def toString: String = s"$name RAM: ${hexAddress(start)}-${hexAddress(end)}"
}

class ROM(val name: String, val start: Int, val end: Int) extends Addressable {
  require(start >= 0, s"ROM start must be ≥ 0, got $start")
  require(end >= start, s"ROM end ($end) must be ≥ start ($start)")

  val size = end - start + 1

  protected[m68k] val mem: Array[Byte] = new Array[Byte](size)

  def readByte(addr: Int): Int = mem(addr - start) & 0xFF

  def writeByte(addr: Int, value: Int): Unit =
    sys.error(s"read only memory: ${hexAddress(addr)} (tried to write ${hexByte(value)})")

  override def programByte(addr: Int, value: Int): Unit = mem(addr - start) = value.toByte

  override def toString: String = s"$name ROM: ${hexAddress(start)}-${hexAddress(end)}"
}

object ROM {
  def apply(name: String, data: Seq[Byte]): ROM = apply(name, 0, data)

  def apply(name: String, start: Int, data: Seq[Byte]): ROM =
    new ROM(name, start, start + data.length - 1) {
      data.copyToArray(mem)
    }

  def code(name: String, start: Int, data: Seq[Int]): ROM = {
    val base = start
    new ROM(name, base, base + data.length * 2 - 1) {
      data.zipWithIndex.foreach { case (inst, idx) => programShort(base + idx * 2, inst) }
    }
  }
}

trait Device extends Addressable {
  protected var cpu: CPU = null

  def connectTo(c: CPU): Unit = {
    cpu = c
    c.resettable(this)
  }

  def init(): Unit  = ()
  def reset(): Unit = ()

  override def toString: String = s"$name device: ${hexAddress(start)}-${hexAddress(start + size - 1)}"
}

abstract class SingleAddressDevice extends Device {
  val size = 1
  override def toString: String = s"$name device: ${hexAddress(start)}"
}

abstract class ReadOnlyDevice extends SingleAddressDevice {
  def writeByte(addr: Int, value: Int): Unit = sys.error("read only device")
}

abstract class WriteOnlyDevice extends SingleAddressDevice {
  def readByte(addr: Int): Int = sys.error("write only device")
}

/** System memory dispatcher.
  *
  * Acceleration: a paged dispatch table indexed by `(addr - first) >> PageShift` maps each page to its owning
  * Addressable. Combined with a one-slot last-region cache (`cachedRegion` + cached `[start, endExcl)` bounds), every
  * load/store is normally either one cached bounds check or one array index — never the linear `indexWhere` scan the
  * original implementation did per byte.
  *
  * The table is rebuilt lazily on the first access after any add/remove, so address-map changes during boot pay no
  * lookup cost.
  */
abstract class Memory extends Addressable {

  val name = "System memory"

  protected val regions = new ArrayBuffer[Addressable]
  protected var first: Int = 0
  protected var endAddr: Int = 0

  // ---- Paged dispatch table -------------------------------------------------

  // 4 KiB pages — small enough that even an MMIO peripheral typically sits in
  // a single page, large enough that a 16 MiB address space takes only 4096
  // entries (32 KiB on a JVM with 8-byte refs). Worst case for a sparse
  // 24-bit address map is bounded; the table covers only [first, endExcl).
  private inline val PageShift = 12
  private inline val PageSize  = 1 << PageShift

  private var pageTable: Array[Addressable] = new Array[Addressable](0)
  private var tableBase: Int                = 0
  private var tableValid: Boolean           = false

  // Last-region cache: avoids even the array index when sequential accesses
  // (instruction fetch, stack, pixel buffer) stay inside the same region.
  private var cachedRegion: Addressable = null
  private var cachedStart: Int          = 0
  private var cachedEndExcl: Int        = 0

  private def rebuildTable(): Unit = {
    if (regions.isEmpty) {
      pageTable = new Array[Addressable](0)
      tableBase = 0
    } else {
      val base = first
      val span = endAddr - base
      val pages = (span + PageSize - 1) >> PageShift
      val table = new Array[Addressable](pages)

      val it = regions.iterator
      while (it.hasNext) {
        val r        = it.next()
        val firstPg  = (r.start - base) >> PageShift
        val lastPg   = (r.start + r.size - 1 - base) >> PageShift
        var p = firstPg
        while (p <= lastPg) {
          table(p) = r
          p += 1
        }
      }

      pageTable = table
      tableBase = base
    }
    tableValid = true
    invalidateCache()
  }

  private def invalidateCache(): Unit = {
    cachedRegion  = null
    cachedStart   = 0
    cachedEndExcl = 0
  }

  /** Hot-path lookup: returns the region containing `addr`, or null. */
  protected[m68k] def lookup(addr: Int): Addressable = {
    val cr = cachedRegion
    if ((cr ne null) && addr >= cachedStart && addr < cachedEndExcl) cr
    else lookupSlow(addr)
  }

  private def lookupSlow(addr: Int): Addressable = {
    if (!tableValid) rebuildTable()
    val rel = addr - tableBase
    if (rel < 0) null
    else {
      val pg = rel >>> PageShift
      if (pg >= pageTable.length) null
      else {
        val r = pageTable(pg)
        if ((r ne null) && r.contains(addr)) {
          cachedRegion  = r
          cachedStart   = r.start
          cachedEndExcl = r.start + r.size
          r
        } else {
          // Page hit but the recorded region doesn't cover this byte — multiple sub-page regions can share one
          // page entry, and rebuildTable only stored the last one. Fall back to a linear scan over the regions
          // in this page; cache whatever we find so subsequent sequential accesses are fast again.
          val rr = scanForAddr(addr)
          if (rr ne null) {
            cachedRegion  = rr
            cachedStart   = rr.start
            cachedEndExcl = rr.start + rr.size
          }
          rr
        }
      }
    }
  }

  private def scanForAddr(addr: Int): Addressable = {
    val it = regions.iterator
    while (it.hasNext) {
      val r = it.next()
      if (r.contains(addr)) return r
    }
    null
  }

  def init(): Unit

  init()

  def valid(addr: Int): Boolean       = lookup(addr) ne null
  def addressable(addr: Int): Boolean = lookup(addr) ne null

  def find(addr: Int): Addressable = lookup(addr) match {
    case null => sys.error(s"${hexAddress(addr)} is not an addressable memory location")
    case r    => r
  }

  def start: Int = first
  def size: Int  = endAddr - first

  // ---- Memory dispatch ------------------------------------------------------

  def readByte(addr: Int): Int               = find(addr).readByte(addr)
  def writeByte(addr: Int, value: Int): Unit = find(addr).writeByte(addr, value)

  override def readShort(addr: Int): Int               = find(addr).readShort(addr)
  override def writeShort(addr: Int, value: Int): Unit = find(addr).writeShort(addr, value)
  override def readInt(addr: Int): Int                 = find(addr).readInt(addr)
  override def writeInt(addr: Int, value: Int): Unit   = find(addr).writeInt(addr, value)
  override def readLong(addr: Int): Long               = find(addr).readLong(addr)
  override def writeLong(addr: Int, value: Long): Unit = find(addr).writeLong(addr, value)

  override def programByte(addr: Int, value: Int): Unit = find(addr).programByte(addr, value)

  // ---- Region-class predicates ----------------------------------------------

  private def findIf(addr: Int, pred: Addressable => Boolean): Boolean = lookup(addr) match {
    case null => false
    case r    => pred(r)
  }

  def device(addr: Int): Boolean = findIf(addr, _.isDevice)
  def memory(addr: Int): Boolean = findIf(addr, r => r.isRAM || r.isROM)
  def rom(addr: Int): Boolean    = findIf(addr, _.isROM)

  // ---- Mutation -------------------------------------------------------------

  def add(region: Addressable): Unit = {
    regions.find(r => r.start <= region.start && region.start < r.start + r.size) match {
      case Some(r) =>
        sys.error(
          s"${hexAddress(region.start)}, ${hexAddress(region.size)} overlaps " +
            s"${hexAddress(r.start)}, ${hexAddress(r.size)}",
        )
      case None =>
    }

    regions.find(_.name == region.name) match {
      case Some(_) => sys.error(s"duplicate region: ${region.name}")
      case None    =>
    }

    val ind = regions.indexWhere(_.start >= region.start + region.size)
    if (ind == -1) {
      val wasEmpty = regions.isEmpty
      regions += region
      if (wasEmpty) first = region.start
      endAddr = region.start + region.size
    } else {
      regions.insert(ind, region)
      if (ind == 0) first = region.start
    }
    tableValid = false
    invalidateCache()
  }

  def remove(name: String): Unit = {
    val ind = regions.indexWhere(_.name == name)
    if (ind == -1) sys.error(s"not found: $name")
    val r = regions(ind)
    if (r.isDevice) r.asInstanceOf[Device].reset()
    regions.remove(ind)
    if (regions.isEmpty) {
      first = 0; endAddr = 0
    } else {
      first = regions.head.start
      endAddr = regions.last.start + regions.last.size
    }
    tableValid = false
    invalidateCache()
  }

  def seqDevice: Seq[Device] = regions.iterator.collect { case d: Device => d }.toSeq
  def seqROM: Seq[ROM]       = regions.iterator.collect { case r: ROM => r }.toSeq
  def seqRAM: Seq[RAM]       = regions.iterator.collect { case r: RAM => r }.toSeq

  def removeROM(): Unit     = seqROM.foreach(r => remove(r.name))
  def removeRAM(): Unit     = seqRAM.filterNot(_.isDevice).foreach(r => remove(r.name))
  def removeDevices(): Unit = seqDevice.foreach(d => remove(d.name))

  def code: Int = seqROM.headOption.map(_.start).getOrElse(0)

  def clearRAM(): Unit = seqRAM.foreach(_.clear())

  def reset(): Unit = seqDevice.foreach(_.init())

  override def toString: String = regions.mkString("\n")
}
