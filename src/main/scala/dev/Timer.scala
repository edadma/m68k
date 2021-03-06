//@
package xyz.hyperreal.m68k.dev

import java.util.{TimerTask, Timer => JTimer}

import xyz.hyperreal.m68k.{MAX_ADDRESS, Interrupt, WriteOnlyDevice}


class Timer( val name: String, level: Int, control: Int ) extends WriteOnlyDevice {

  val start = control&MAX_ADDRESS
  var timer: JTimer = null
  var running = false
  val timerTask =
    new TimerTask {
      def run: Unit = {
        cpu.interrupt( new Interrupt(level, None) )
      }
    }

  override def reset: Unit = {
    if (running) {
      timer.cancel
      timer = null
      running = false
    }
  }

	def writeByte( addr: Int, value: Int ): Unit = {
    value match {
      case 0 => reset
      case ms =>
        timer = new JTimer
        timer.scheduleAtFixedRate( timerTask, 0, ms&0xFF )
        running = true
    }
  }

}