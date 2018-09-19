package xyz.hyperreal.m68k

import scala.collection.mutable.HashMap


object MapFileReader extends App {

  println( apply(io.Source.fromFile("test/main.map")) )

  lazy val labelRegex = " {16}0x([^ ]+) {16}([^ ]+).*"r

  def apply( src: io.Source ): (HashMap[String, Long], HashMap[Long, String]) = {
    val symbols = new HashMap[String, Long]
    var started = false

    for (line <- src.getLines) {
      if (started) {
        if (line.nonEmpty) {
          if (!line.contains( ' ' ))
            return (symbols, symbols map {case (d, r) => (r, d)})
          else {
            line match {
              case labelRegex( address, label ) => symbols(label) = java.lang.Long.parseLong( address, 16 )
              case _ =>
            }
          }
        }
      } else if (line == "Linker script and memory map")
        started = true
    }

    sys.error( "error reading map file" )
  }

}