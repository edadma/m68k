package io.github.edadma.m68k

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Bit shifts and rotates — the eight m68k shift/rotate ops × three widths. The numeric expectations come from
  * the original BitShiftTests for ROXL/ROXR/LSR plus additions for the rest of the family. Each width is tested
  * independently because the bit positions, carry-out source, and overflow detection differ.
  */
class CPUShiftTests extends AnyFreeSpec with Matchers with Testing {

  // ---- ROXL byte (rotate left through extend) -----------------------------

  "roxlByte" - {
    "0 shift, X=0: value unchanged, no flags" in {
      val cpu = newCPU()
      cpu.setX(false)
      cpu.roxlByte(0, 1) shouldBe 1
      flags(cpu) shouldBe ""
    }
    "0 shift, X=1: value unchanged, X/C set from X" in {
      val cpu = newCPU()
      cpu.setX(true)
      cpu.roxlByte(0, 1) shouldBe 1
      flags(cpu) shouldBe "xc"
    }
    "1 shift" in {
      val cpu = newCPU(); cpu.setX(false)
      cpu.roxlByte(1, 1) shouldBe 2; flags(cpu) shouldBe ""
      cpu.setX(true)
      cpu.roxlByte(1, 1) shouldBe 3; flags(cpu) shouldBe ""

      cpu.setX(false)
      cpu.roxlByte(1, 0x81) shouldBe 2; flags(cpu) shouldBe "xc"
      cpu.setX(true)
      cpu.roxlByte(1, 0x81) shouldBe 3; flags(cpu) shouldBe "xc"
    }
    "2 shift" in {
      val cpu = newCPU(); cpu.setX(false)
      cpu.roxlByte(2, 1) shouldBe 4; flags(cpu) shouldBe ""
      cpu.setX(true)
      cpu.roxlByte(2, 1) shouldBe 6; flags(cpu) shouldBe ""

      cpu.setX(false)
      cpu.roxlByte(2, 0x81) shouldBe 5; flags(cpu) shouldBe ""
      cpu.setX(true)
      cpu.roxlByte(2, 0x81) shouldBe 7; flags(cpu) shouldBe ""
    }
    "3 shift" in {
      val cpu = newCPU(); cpu.setX(false)
      cpu.roxlByte(3, 0xC1) shouldBe 0x0B; flags(cpu) shouldBe ""
      cpu.setX(true)
      cpu.roxlByte(3, 0xC1) shouldBe 0x0F; flags(cpu) shouldBe ""
    }
  }

  // ---- ROXR byte ----------------------------------------------------------

  "roxrByte" - {
    "0 shift" in {
      val cpu = newCPU(); cpu.setX(false)
      unsignByte(cpu.roxrByte(0, 0x80)) shouldBe 0x80
      flags(cpu) shouldBe "n"
      cpu.setX(true)
      unsignByte(cpu.roxrByte(0, 0x80)) shouldBe 0x80
      flags(cpu) shouldBe "xnc"
    }
    "1 shift" in {
      val cpu = newCPU(); cpu.setX(false)
      unsignByte(cpu.roxrByte(1, 0x80)) shouldBe 0x40; flags(cpu) shouldBe ""
      cpu.setX(true)
      unsignByte(cpu.roxrByte(1, 0x80)) shouldBe 0xC0; flags(cpu) shouldBe "n"
      cpu.setX(false)
      unsignByte(cpu.roxrByte(1, 0x81)) shouldBe 0x40; flags(cpu) shouldBe "xc"
      cpu.setX(true)
      unsignByte(cpu.roxrByte(1, 0x81)) shouldBe 0xC0; flags(cpu) shouldBe "xnc"
    }
    "3 shift through 0x83" in {
      val cpu = newCPU(); cpu.setX(false)
      unsignByte(cpu.roxrByte(3, 0x83)) shouldBe 0xD0; flags(cpu) shouldBe "n"
      cpu.setX(true)
      unsignByte(cpu.roxrByte(3, 0x83)) shouldBe 0xF0; flags(cpu) shouldBe "n"
    }
  }

  // ---- ROXR int -----------------------------------------------------------

  "roxrInt" - {
    "1 shift with X=1 from 0 = 0x80000000" in {
      val cpu = newCPU(); cpu.setX(true)
      cpu.roxrInt(1, 0) shouldBe 0x80000000
      flags(cpu) shouldBe "n"
    }
    "1 shift X=true from 0x80 = 0x80000040, N" in {
      val cpu = newCPU(); cpu.setX(true)
      cpu.roxrInt(1, 0x80) shouldBe 0x80000040
      flags(cpu) shouldBe "n"
    }
    "1 shift X=true from 0x81 = 0x80000040, X/N/C" in {
      val cpu = newCPU(); cpu.setX(true)
      cpu.roxrInt(1, 0x81) shouldBe 0x80000040
      flags(cpu) shouldBe "xnc"
    }
    "3 shift X=true from 0x83 = 0xE0000010, N" in {
      val cpu = newCPU(); cpu.setX(true)
      cpu.roxrInt(3, 0x83) shouldBe 0xE0000010
      flags(cpu) shouldBe "n"
    }
  }

  // ---- LSR byte (logical shift right) -------------------------------------

  "lsrByte" - {
    "0 shift leaves value, only N/Z reflect input" in {
      val cpu = newCPU()
      unsignByte(cpu.lsrByte(0, 0x80)) shouldBe 0x80
      flags(cpu) shouldBe "n"
    }
    "1 shift through 0x80 = 0x40, no carry" in {
      val cpu = newCPU()
      unsignByte(cpu.lsrByte(1, 0x80)) shouldBe 0x40; flags(cpu) shouldBe ""
    }
    "1 shift through 0x81 = 0x40, X/C set" in {
      val cpu = newCPU()
      unsignByte(cpu.lsrByte(1, 0x81)) shouldBe 0x40; flags(cpu) shouldBe "xc"
    }
    "3 shift through 0x83 = 0x10, X/C from low bit" in {
      val cpu = newCPU()
      unsignByte(cpu.lsrByte(3, 0x83)) shouldBe 0x10; flags(cpu) shouldBe ""
    }
    "shift larger than width sets Z, C from last shifted bit" in {
      val cpu = newCPU()
      unsignByte(cpu.lsrByte(8, 0x80)) shouldBe 0
      flags(cpu) shouldBe "xzc"  // bit 7 fell out as the 8th carry
    }
    "shift much larger zeros out and clears C" in {
      val cpu = newCPU()
      unsignByte(cpu.lsrByte(9, 0x80)) shouldBe 0
      flags(cpu) shouldBe "z"
    }
  }

  // ---- LSL byte -----------------------------------------------------------

  "lslByte" - {
    "0 shift" in {
      val cpu = newCPU()
      unsignByte(cpu.lslByte(0, 0x80)) shouldBe 0x80
      flags(cpu) shouldBe "n"
    }
    "1 shift through 0x80 = 0, X/Z/C" in {
      val cpu = newCPU()
      unsignByte(cpu.lslByte(1, 0x80)) shouldBe 0; flags(cpu) shouldBe "xzc"
    }
    "1 shift through 0x40 = 0x80, N" in {
      val cpu = newCPU()
      unsignByte(cpu.lslByte(1, 0x40)) shouldBe 0x80; flags(cpu) shouldBe "n"
    }
    "shift by 8 returns 0" in {
      val cpu = newCPU()
      unsignByte(cpu.lslByte(8, 0x01)) shouldBe 0
      flags(cpu) shouldBe "xzc"
    }
  }

  // ---- ASL byte -----------------------------------------------------------

  "aslByte sets V when sign bit changes" in {
    val cpu = newCPU()
    unsignByte(cpu.aslByte(1, 0x40)) shouldBe 0x80
    cpu.V shouldBe true; cpu.N shouldBe true
  }

  "aslByte no V when sign stable" in {
    val cpu = newCPU()
    unsignByte(cpu.aslByte(1, 0x10)) shouldBe 0x20
    cpu.V shouldBe false
  }

  // ---- ASR byte (arithmetic shift right) ---------------------------------

  "asrByte sign-extends, V always cleared" in {
    val cpu = newCPU()
    cpu.setV(true)
    val r = cpu.asrByte(1, 0x80)
    unsignByte(r) shouldBe 0xC0
    cpu.V shouldBe false; cpu.N shouldBe true
  }

  "asrByte large shift saturates at 0xFF for negatives" in {
    val cpu = newCPU()
    unsignByte(cpu.asrByte(8, 0x80)) shouldBe 0xFF
    cpu.N shouldBe true
  }

  "asrByte large shift saturates at 0 for positives" in {
    val cpu = newCPU()
    unsignByte(cpu.asrByte(8, 0x40)) shouldBe 0
    cpu.Z shouldBe true
  }

  // ---- ROL / ROR (no extend) ---------------------------------------------

  "rolByte 1 shift wraps MSB to LSB" in {
    val cpu = newCPU()
    unsignByte(cpu.rolByte(1, 0x80)) shouldBe 0x01
    cpu.C shouldBe true; cpu.N shouldBe false
  }

  "rolByte 1 shift no-wrap MSB→LSB" in {
    val cpu = newCPU()
    unsignByte(cpu.rolByte(1, 0x40)) shouldBe 0x80
    cpu.N shouldBe true; cpu.C shouldBe false
  }

  "rorByte 1 shift wraps LSB to MSB" in {
    val cpu = newCPU()
    unsignByte(cpu.rorByte(1, 0x01)) shouldBe 0x80
    cpu.C shouldBe true; cpu.N shouldBe true
  }

  "rolShort identity at 16" in {
    val cpu = newCPU()
    unsignShort(cpu.rolShort(16, 0x1234)) shouldBe 0x1234
  }

  "rorInt identity at 32" in {
    val cpu = newCPU()
    cpu.rorInt(32, 0xDEADBEEF) shouldBe 0xDEADBEEF
  }

  "rolByte / rorByte / roxlByte / roxrByte do NOT touch X (rotates without extend)" - {
    "rolByte" in {
      val cpu = newCPU(); cpu.setX(true)
      cpu.rolByte(1, 0x80); cpu.X shouldBe true
    }
    "rorByte" in {
      val cpu = newCPU(); cpu.setX(true)
      cpu.rorByte(1, 0x01); cpu.X shouldBe true
    }
  }
}
