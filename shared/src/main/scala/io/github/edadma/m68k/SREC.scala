package io.github.edadma.m68k

import scala.collection.mutable.ArrayBuffer

/** Motorola S-record (SREC) reader/writer.
  *
  * Parsing builds one ROM region per contiguous data run and registers it with the supplied [[Memory]]. Returns the
  * start address declared by the S9 record (S-record entry point), which the caller typically writes to the reset
  * vector.
  *
  * Supports record types S0 (header), S1 (16-bit address data), S5 (count check), S9 (start). S2/S3 (24/32-bit
  * address data) and their S7/S8 start records aren't emitted by m68k toolchains for the 16-bit-address subset and
  * aren't accepted yet.
  */
object SREC {

  // ----- Reader --------------------------------------------------------------

  def read(m: Memory, source: String): Int = {
    var startAddr   = 0
    val buf         = new ArrayBuffer[Byte]
    var base        = 0
    var segment     = 0

    def emit(): Unit =
      if buf.nonEmpty then {
        m.add(ROM("SREC" + segment, base, buf.toIndexedSeq))
        segment += 1
        buf.clear()
      }

    def data(addr: Int, bytes: IndexedSeq[Byte]): Unit = {
      if buf.nonEmpty && addr != base + buf.length then {
        emit()
        base = addr
      } else if buf.isEmpty then base = addr
      bytes.foreach(b => buf += b)
    }

    parse(
      source,
      _ => (),
      data,
      a => startAddr = a,
    )
    emit()
    startAddr
  }

  /** Lower-level streaming parser. Calls back for each record kind so callers that don't want ROM regions (e.g. a
    * disassembler that wants raw bytes) can use it too.
    */
  def parse(
      source: String,
      header: IndexedSeq[Byte] => Unit,
      data: (Int, IndexedSeq[Byte]) => Unit,
      start: Int => Unit,
  ): Unit = {
    var headerSeen = false
    var count      = 0

    val lines = source.linesIterator.zipWithIndex
    while lines.hasNext do {
      val (line0, num) = lines.next()
      val line         = line0.trim

      def problem(col: Int, msg: String): Nothing =
        sys.error(s"SREC error on line ${num + 1}: $msg\n$line\n${" " * col}^")

      def hexb(index: Int): Int = {
        var i = 0
        while i < 2 do {
          if !"0123456789abcdefABCDEF".contains(line(index + i)) then
            problem(index + i, "non-hexadecimal character")
          i += 1
        }
        Integer.parseInt(line.substring(index, index + 2), 16)
      }

      if line.nonEmpty then {
        if line.length < 10 then problem(0, "line too short")
        if line.length % 2 != 0 then problem(0, "line has an odd number of characters")
        if line(0) != 'S' then problem(0, "expected 'S'")

        val binary = (2 until line.length - 2 by 2).map(i => hexb(i).toByte).toIndexedSeq

        def byte(index: Int): Int = binary(index) & 0xFF
        def word(index: Int): Int = (byte(index) << 8) | byte(index + 1)

        val sum = binary.foldLeft(0)((s, b) => s + (b & 0xFF))
        if ((~sum) & 0xFF) != hexb(line.length - 2) then problem(line.length - 2, "incorrect checksum")
        if binary(0) != binary.length then problem(2, "incorrect count")

        line(1) match {
          case '0' =>
            if headerSeen then problem(1, "duplicate header record")
            if word(1) != 0 then problem(4, "address field should be 0000 for header")
            headerSeen = true
            header(binary.drop(3))
          case '1' =>
            count += 1
            data(word(1), binary.drop(3))
          case '5' =>
            if count != word(1) then problem(9, "incorrect record count")
          case '9' => start(word(1))
          case _   => problem(1, s"unsupported record type 'S${line(1)}'")
        }
      }
    }
  }

  // ----- Writer --------------------------------------------------------------

  /** Render every ROM region in `m` as an S-record string suitable for writing to a file. */
  def write(m: Memory, header: Seq[Byte] = Nil): String = {
    val sb = new StringBuilder
    var s1count = 0

    def record(typ: Char, items: Seq[Int]): Unit = {
      val count = items.length + 1
      val sum   = items.foldLeft(count)(_ + _) & 0xFF
      sb.append('S').append(typ).append(hexByte(count))
      items.foreach(b => sb.append(hexByte(b)))
      sb.append(hexByte(sum ^ 0xFF))
      sb.append('\n')
    }

    record('0', Seq(0x00, 0x00) ++ header.map(_.toInt & 0xFF))

    val roms = m.seqROM
    if roms.isEmpty then sys.error("there's no ROM to be saved")

    for rom <- roms do {
      val end = rom.start + rom.size
      var rec = rom.start
      while rec < end do {
        val len   = math.min(16, end - rec)
        val bytes = (0 until len).map(i => rom.readByte(rec + i))
        record('1', Seq((rec >> 8) & 0xFF, rec & 0xFF) ++ bytes)
        s1count += 1
        rec += 16
      }
    }

    record('5', Seq((s1count >> 8) & 0xFF, s1count & 0xFF))
    record('9', Seq(0, 0))
    sb.toString
  }
}
