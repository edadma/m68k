//@
package xyz.hyperreal.m68k

import java.io.File


object Main extends App {
  val mem =
    new Memory {
      def init: Unit = {
        regions.clear
        add( new RAM("ram", 0x10000, 0x1FFFF) )
        SREC( this, new File("tools/int2stru.srec") )
      }
    }
  val cpu =
    new CPUWithServices( mem ) {
//      trace = true
      symbols = MapFileReader( io.Source.fromFile("tools/int2stru.map") )._2
    }

  cpu.reset
  cpu.run
}