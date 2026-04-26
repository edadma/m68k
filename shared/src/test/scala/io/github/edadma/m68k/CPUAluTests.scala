package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** ALU primitives — width-specialised (no Size param). Edge cases per width: zero result, negative result,
  * unsigned carry-out, signed overflow, and the X-bit "extended" carry chain (ADDX/SUBX/NEGX) where Z is sticky.
  */
class CPUAluTests extends AnyFreeSpec with Matchers with Testing {

  // ----- ADD ----------------------------------------------------------------

  "addByte" - {
    "1 + 2 = 3, no flags" in {
      val cpu = newCPU()
      cpu.addByte(1, 2) shouldBe 3
      flags(cpu) shouldBe ""
    }
    "0 + 0 = 0, Z set" in {
      val cpu = newCPU()
      cpu.addByte(0, 0) shouldBe 0
      flags(cpu) shouldBe "z"
    }
    "0xFF + 1 wraps to 0, X/Z/C set" in {
      val cpu = newCPU()
      cpu.addByte(0xFF, 1) shouldBe 0
      flags(cpu) shouldBe "xzc"
    }
    "0x7F + 1 = 0x80, V/N set (signed overflow)" in {
      val cpu = newCPU()
      cpu.addByte(0x7F, 1) shouldBe 0x80
      flags(cpu) shouldBe "nv"
    }
    "0x80 + 0x80 = 0, X/Z/V/C set" in {
      val cpu = newCPU()
      cpu.addByte(0x80, 0x80) shouldBe 0
      flags(cpu) shouldBe "xzvc"
    }
    "negative + negative no overflow keeps N" in {
      val cpu = newCPU()
      cpu.addByte(0xF0, 0xF0) shouldBe 0xE0 // wraps with carry
      flags(cpu) shouldBe "xnc"
    }
  }

  "addShort" - {
    "0xFFFF + 1 = 0, X/Z/C" in {
      val cpu = newCPU()
      cpu.addShort(0xFFFF, 1) shouldBe 0
      flags(cpu) shouldBe "xzc"
    }
    "0x7FFF + 1 = 0x8000, N/V" in {
      val cpu = newCPU()
      cpu.addShort(0x7FFF, 1) shouldBe 0x8000
      flags(cpu) shouldBe "nv"
    }
  }

  "addInt" - {
    "0xFFFFFFFF + 1 = 0, X/Z/C" in {
      val cpu = newCPU()
      cpu.addInt(-1, 1) shouldBe 0
      flags(cpu) shouldBe "xzc"
    }
    "Int.MaxValue + 1 = Int.MinValue, N/V" in {
      val cpu = newCPU()
      cpu.addInt(Int.MaxValue, 1) shouldBe Int.MinValue
      flags(cpu) shouldBe "nv"
    }
  }

  // ----- ADDX (X carries from prior op; Z is sticky) ------------------------

  "addxByte chains carry through X" in {
    val cpu = newCPU()
    cpu.setX(true); cpu.setZ(true)  // simulate prior result was 0
    // 0 + 0 + 1 = 1: not zero ⇒ Z must clear
    cpu.addxByte(0, 0) shouldBe 1
    cpu.Z shouldBe false
    cpu.X shouldBe false
    cpu.C shouldBe false
  }

  "addxByte: 0xFF + 0 + 1 = 0, sets X/C, keeps Z if it was set" in {
    val cpu = newCPU()
    cpu.setX(true); cpu.setZ(true)
    cpu.addxByte(0xFF, 0) shouldBe 0
    cpu.Z shouldBe true   // sticky — was set, result is zero, stays set
    cpu.X shouldBe true
    cpu.C shouldBe true
  }

  "addxByte: 0xFF + 0 + 1 = 0 with Z initially clear leaves Z clear" in {
    val cpu = newCPU()
    cpu.setX(true); cpu.setZ(false)
    cpu.addxByte(0xFF, 0) shouldBe 0
    cpu.Z shouldBe false
  }

  "addxInt 32-bit overflow chains correctly" in {
    val cpu = newCPU()
    cpu.setX(true); cpu.setZ(true)
    cpu.addxInt(-1, 0) shouldBe 0
    cpu.X shouldBe true; cpu.C shouldBe true; cpu.Z shouldBe true
  }

  // ----- SUB ----------------------------------------------------------------

  "subByte" - {
    "5 - 3 = 2" in {
      val cpu = newCPU()
      cpu.subByte(3, 5) shouldBe 2
      flags(cpu) shouldBe ""
    }
    "5 - 5 = 0, Z" in {
      val cpu = newCPU()
      cpu.subByte(5, 5) shouldBe 0
      flags(cpu) shouldBe "z"
    }
    "0 - 1 = 0xFF, X/N/C (borrow)" in {
      val cpu = newCPU()
      cpu.subByte(1, 0) shouldBe 0xFF
      flags(cpu) shouldBe "xnc"
    }
    "0x80 - 1 = 0x7F, V (signed underflow)" in {
      val cpu = newCPU()
      cpu.subByte(1, 0x80) shouldBe 0x7F
      flags(cpu) shouldBe "v"
    }
    "0x7F - 0xFF = 0x80 with borrow, V/N/X/C" in {
      val cpu = newCPU()
      cpu.subByte(0xFF, 0x7F) shouldBe 0x80
      flags(cpu) shouldBe "xnvc"
    }
  }

  "subShort underflow" in {
    val cpu = newCPU()
    cpu.subShort(1, 0) shouldBe 0xFFFF
    flags(cpu) shouldBe "xnc"
  }

  "subInt MinValue - 1 = MaxValue, V (signed underflow), no borrow" in {
    // 0x80000000 - 1 = 0x7FFFFFFF: in *unsigned* terms 2³¹ - 1 = 2³¹-1, no borrow.
    val cpu = newCPU()
    cpu.subInt(1, Int.MinValue) shouldBe Int.MaxValue
    flags(cpu) shouldBe "v"
  }

  "subInt with actual unsigned borrow sets X/C" in {
    val cpu = newCPU()
    cpu.subInt(1, 0) shouldBe -1
    flags(cpu) shouldBe "xnc"
  }

  // ----- SUBX ---------------------------------------------------------------

  "subxByte borrows through X with sticky Z" in {
    val cpu = newCPU()
    cpu.setX(true); cpu.setZ(true)
    cpu.subxByte(0, 1) shouldBe 0   // 1 - 0 - 1 = 0
    cpu.Z shouldBe true   // sticky
    cpu.X shouldBe false; cpu.C shouldBe false
  }

  "subxByte 0 - 0 with X = 1 = 0xFF with X/N/C, Z clear" in {
    val cpu = newCPU()
    cpu.setX(true); cpu.setZ(true)
    cpu.subxByte(0, 0) shouldBe 0xFF
    cpu.X shouldBe true; cpu.C shouldBe true; cpu.N shouldBe true
    cpu.Z shouldBe false  // result non-zero ⇒ Z clears even sticky
  }

  // ----- CMP (no result, X unchanged) ---------------------------------------

  "cmpByte sets flags but leaves X alone" in {
    val cpu = newCPU()
    cpu.setX(true)
    cpu.cmpByte(5, 5) // d - s = 0
    flags(cpu) shouldBe "xz"  // X preserved
  }

  "cmpInt: 5 cmp 3 — borrow/N because 3 - 5 = -2" in {
    val cpu = newCPU()
    cpu.cmpInt(5, 3)
    flags(cpu) shouldBe "nc"
  }

  // ----- NEG / NEGX ---------------------------------------------------------

  "negByte" - {
    "neg 0 = 0, Z, no carry" in {
      val cpu = newCPU()
      cpu.negByte(0) shouldBe 0
      flags(cpu) shouldBe "z"
    }
    "neg 1 = 0xFF, X/N/C" in {
      val cpu = newCPU()
      cpu.negByte(1) shouldBe 0xFF
      flags(cpu) shouldBe "xnc"
    }
    "neg 0x80 = 0x80, V/N/X/C (only value where overflow occurs)" in {
      val cpu = newCPU()
      cpu.negByte(0x80) shouldBe 0x80
      flags(cpu) shouldBe "xnvc"
    }
  }

  "negShort 0x8000 overflows" in {
    val cpu = newCPU()
    cpu.negShort(0x8000) shouldBe 0x8000
    flags(cpu) shouldBe "xnvc"
  }

  "negInt Int.MinValue overflows" in {
    val cpu = newCPU()
    cpu.negInt(Int.MinValue) shouldBe Int.MinValue
    flags(cpu) shouldBe "xnvc"
  }

  "negxByte: -0 - 1 with X = -1, X/N/C" in {
    val cpu = newCPU()
    cpu.setX(true); cpu.setZ(true)
    cpu.negxByte(0) shouldBe 0xFF
    cpu.X shouldBe true; cpu.N shouldBe true; cpu.C shouldBe true
    cpu.Z shouldBe false
  }

  "negxByte: -0 - 0 with X clear = 0, no flags but Z sticky" in {
    val cpu = newCPU()
    cpu.setX(false); cpu.setZ(true)
    cpu.negxByte(0) shouldBe 0
    cpu.Z shouldBe true
  }

  // ----- AND / OR / EOR / NOT (logic ops: V/C cleared, N/Z set) -------------

  "andByte sets N/Z, clears V/C, leaves X" in {
    val cpu = newCPU()
    cpu.setX(true); cpu.setV(true); cpu.setC(true)
    cpu.andByte(0xF0, 0x0F) shouldBe 0
    flags(cpu) shouldBe "xz"
  }

  "andByte negative" in {
    val cpu = newCPU()
    cpu.andByte(0xF0, 0x80) shouldBe 0x80
    flags(cpu) shouldBe "n"
  }

  "orByte combines bits" in {
    val cpu = newCPU()
    cpu.orByte(0xF0, 0x0F) shouldBe 0xFF
    flags(cpu) shouldBe "n"
  }

  "eorByte" in {
    val cpu = newCPU()
    cpu.eorByte(0xFF, 0x0F) shouldBe 0xF0
    flags(cpu) shouldBe "n"
  }

  "notByte" in {
    val cpu = newCPU()
    cpu.notByte(0x00) shouldBe 0xFF
    flags(cpu) shouldBe "n"
    cpu.notByte(0xFF) shouldBe 0x00
    cpu.Z shouldBe true
  }

  "notShort" in {
    val cpu = newCPU()
    cpu.notShort(0x0000) shouldBe 0xFFFF
    flags(cpu) shouldBe "n"
  }

  "notInt" in {
    val cpu = newCPU()
    cpu.notInt(0) shouldBe -1
    flags(cpu) shouldBe "n"
  }

  // ----- TST ---------------------------------------------------------------

  "tstByte" in {
    val cpu = newCPU()
    cpu.setV(true); cpu.setC(true); cpu.setX(true)
    cpu.tstByte(0)
    flags(cpu) shouldBe "xz"
    cpu.tstByte(0x80)
    flags(cpu) shouldBe "xn"
    cpu.tstByte(0x42)
    flags(cpu) shouldBe "x"
  }

  "tstShort" in {
    val cpu = newCPU()
    cpu.tstShort(0x8000)
    flags(cpu) shouldBe "n"
  }

  "tstInt" in {
    val cpu = newCPU()
    cpu.tstInt(Int.MinValue)
    flags(cpu) shouldBe "n"
    cpu.tstInt(0)
    flags(cpu) shouldBe "z"
  }
}
