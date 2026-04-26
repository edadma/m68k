package io.github.edadma.m68k

import scala.collection.mutable.ListBuffer

/** 64 KiB opcode dispatch table.
  *
  * Built once at first access via the `populate(pattern, ctor)` DSL ported from the original. Patterns are bit
  * strings with variable letters (e.g., `"1101 rrr 0 ss eee aaa; s:0-2"`); `generate` enumerates every matching
  * 16-bit opcode and the constructor receives the variable values as a Map. Width-parameterised opcodes use a
  * size-switch in the constructor lambda to pick the right per-width Instruction subclass.
  *
  * The whole table is filled at class-load and never re-walked. `apply(opcode)` is one array index plus one
  * virtual call.
  */
object OpcodeTable {

  private val table = new Array[Instruction](0x10000)
  java.util.Arrays.fill(table.asInstanceOf[Array[AnyRef]], ILLEGAL)

  // The size selector for ADD / SUB / CMP / AND / OR / etc. — bits 7..6 of the opcode. */
  private inline def addqsize(o: Map[Char, Int]): Int = o('s') // 0 → byte, 1 → short, 2 → int

  // ADDA / SUBA / CMPA: bit 8 selects W (=3) or L (=7).
  private inline def isLongA(o: Map[Char, Int]): Boolean = o('s') == 7

  // EXT: 010 → W, 011 → L
  private inline def isLongExt(o: Map[Char, Int]): Boolean = o('s') == 3

  // MOVE: bits 13..12 — 1=B, 3=W, 2=L (m68k's funny ordering).
  private inline def moveSize(o: Map[Char, Int]): Int = o('s') match {
    case 1 => 0; case 3 => 1; case 2 => 2
  }

  // CHK / MOVEA: bit 9 — 3=W, 2=L
  private inline def isLongChk(o: Map[Char, Int]): Boolean = o('s') == 2

  // MOVEM: bit 6 — 0=W, 1=L
  private inline def isLongMovem(o: Map[Char, Int]): Boolean = o('s') == 1

  private def populate(pattern: String, ctor: Map[Char, Int] => Instruction): Unit =
    for ((idx, m) <- generate(pattern)) table(idx) = ctor(m)

  private def populateAll(specs: Seq[(String, Map[Char, Int] => Instruction)]): Unit =
    specs.foreach { case (p, c) => populate(p, c) }

  // Expose the populated table as a value (built once, on first access).
  private lazy val built: Array[Instruction] = {
    populateAll(Seq(
      "1100 yyy 10000 r xxx" -> (o => new ABCD(o('y'), o('r'), o('x'))),

      "1101 rrr 0 ss eee aaa; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new ADDByteToD(o('r'), o('e'), o('a'))
          case 1 => new ADDShortToD(o('r'), o('e'), o('a'))
          case 2 => new ADDIntToD(o('r'), o('e'), o('a'))
        }),
      "1101 rrr 1 ss eee aaa; s:0-2; e:2-7" ->
        (o => addqsize(o) match {
          case 0 => new ADDByteToEA(o('r'), o('e'), o('a'))
          case 1 => new ADDShortToEA(o('r'), o('e'), o('a'))
          case 2 => new ADDIntToEA(o('r'), o('e'), o('a'))
        }),
      "1101 rrr sss eee aaa; s:3,7" ->
        (o => if isLongA(o) then new ADDAInt(o('r'), o('e'), o('a')) else new ADDAShort(o('r'), o('e'), o('a'))),

      "00000110 ss eee aaa; s:0-2; e:0-7-1" ->
        (o => addqsize(o) match {
          case 0 => new ADDIByte(o('e'), o('a'))
          case 1 => new ADDIShort(o('e'), o('a'))
          case 2 => new ADDIInt(o('e'), o('a'))
        }),
      "0101 ddd 0 ss eee aaa; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new ADDQByte(addqdata(o), o('e'), o('a'))
          case 1 => new ADDQShort(addqdata(o), o('e'), o('a'))
          case 2 => new ADDQInt(addqdata(o), o('e'), o('a'))
        }),
      "1101 xxx 1 ss 00 m yyy; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new ADDXByte(o('x'), o('m'), o('y'))
          case 1 => new ADDXShort(o('x'), o('m'), o('y'))
          case 2 => new ADDXInt(o('x'), o('m'), o('y'))
        }),

      "1100 rrr 0 ss eee aaa; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new ANDByteToD(o('r'), o('e'), o('a'))
          case 1 => new ANDShortToD(o('r'), o('e'), o('a'))
          case 2 => new ANDIntToD(o('r'), o('e'), o('a'))
        }),
      "1100 rrr 1 ss eee aaa; s:0-2; e:2-7" ->
        (o => addqsize(o) match {
          case 0 => new ANDByteToEA(o('r'), o('e'), o('a'))
          case 1 => new ANDShortToEA(o('r'), o('e'), o('a'))
          case 2 => new ANDIntToEA(o('r'), o('e'), o('a'))
        }),
      "00000010 ss eee aaa; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new ANDIByte(o('e'), o('a'))
          case 1 => new ANDIShort(o('e'), o('a'))
          case 2 => new ANDIInt(o('e'), o('a'))
        }),
      "0000001000111100" -> (_ => ANDItoCCR),
      "0000001001111100" -> (_ => ANDItoSR),

      "1110000 d 11 eee aaa; e:2-7" ->
        (o => if o('d') == 0 then new ASRMem(o('e'), o('a')) else new ASLMem(o('e'), o('a'))),
      "1110 ccc d ss i 00 rrr; s:0-2" ->
        (o => if o('d') == 0 then
          addqsize(o) match {
            case 0 => new ASRByteReg(o('c'), o('i'), o('r'))
            case 1 => new ASRShortReg(o('c'), o('i'), o('r'))
            case 2 => new ASRIntReg(o('c'), o('i'), o('r'))
          }
        else
          addqsize(o) match {
            case 0 => new ASLByteReg(o('c'), o('i'), o('r'))
            case 1 => new ASLShortReg(o('c'), o('i'), o('r'))
            case 2 => new ASLIntReg(o('c'), o('i'), o('r'))
          }),

      "0110 cccc dddddddd" -> (o => new Bcc(o('c'), o('d').toByte)),

      "0000 rrr 101 eee aaa" -> (o => new BCHG(o('r'), o('e'), o('a'))),
      "0000100001 eee aaa"   -> (o => new BCHG(-1, o('e'), o('a'))),
      "0000 rrr 110 eee aaa" -> (o => new BCLR(o('r'), o('e'), o('a'))),
      "0000100010 eee aaa"   -> (o => new BCLR(-1, o('e'), o('a'))),
      "0000 rrr 111 eee aaa" -> (o => new BSET(o('r'), o('e'), o('a'))),
      "0000100011 eee aaa"   -> (o => new BSET(-1, o('e'), o('a'))),
      "0000 rrr 100 eee aaa" -> (o => new BTST(o('r'), o('e'), o('a'))),
      "0000100000 eee aaa"   -> (o => new BTST(-1, o('e'), o('a'))),

      "0100100001001 vvv" -> (o => new BKPT(o('v'))),
      "01100001 dddddddd" -> (o => new BSR(o('d').toByte)),

      "0100 rrr ss 0 eee aaa; s:2,3" ->
        (o => if isLongChk(o) then new CHKInt(o('r'), o('e'), o('a')) else new CHKShort(o('r'), o('e'), o('a'))),

      "01000010 ss eee aaa; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new CLRByte(o('e'), o('a'))
          case 1 => new CLRShort(o('e'), o('a'))
          case 2 => new CLRInt(o('e'), o('a'))
        }),

      "1011 rrr sss eee aaa; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new CMPByte(o('r'), o('e'), o('a'))
          case 1 => new CMPShort(o('r'), o('e'), o('a'))
          case 2 => new CMPInt(o('r'), o('e'), o('a'))
        }),
      "1011 rrr sss eee aaa; s:3,7" ->
        (o => if isLongA(o) then new CMPAInt(o('r'), o('e'), o('a')) else new CMPAShort(o('r'), o('e'), o('a'))),
      "00001100 ss eee aaa; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new CMPIByte(o('e'), o('a'))
          case 1 => new CMPIShort(o('e'), o('a'))
          case 2 => new CMPIInt(o('e'), o('a'))
        }),
      "1011 xxx 1 ss 001 yyy; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new CMPMByte(o('x'), o('y'))
          case 1 => new CMPMShort(o('x'), o('y'))
          case 2 => new CMPMInt(o('x'), o('y'))
        }),

      "0101 cccc 11001 rrr" -> (o => new DBcc(o('c'), o('r'))),
      "1000 rrr 111 eee aaa; e:0-7-1" -> (o => new DIVS(o('r'), o('e'), o('a'))),
      "1000 rrr 011 eee aaa; e:0-7-1" -> (o => new DIVU(o('r'), o('e'), o('a'))),

      "1011 rrr 1 ss eee aaa; s:0-2; e:0-7-1" ->
        (o => addqsize(o) match {
          case 0 => new EORByte(o('r'), o('e'), o('a'))
          case 1 => new EORShort(o('r'), o('e'), o('a'))
          case 2 => new EORInt(o('r'), o('e'), o('a'))
        }),
      "00001010 ss eee aaa; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new EORIByte(o('e'), o('a'))
          case 1 => new EORIShort(o('e'), o('a'))
          case 2 => new EORIInt(o('e'), o('a'))
        }),
      "0000101000111100" -> (_ => EORItoCCR),
      "0000101001111100" -> (_ => EORItoSR),

      "1100 xxx 1 ooooo yyy; o:8,9,17" -> (o => new EXG(o('x'), o('o'), o('y'))),
      "01001000 ss 000 rrr; s:2,3" ->
        (o => if isLongExt(o) then new EXTInt(o('r')) else new EXTShort(o('r'))),

      "0100111011 eee aaa" -> (o => new JMP(o('e'), o('a'))),
      "0100111010 eee aaa" -> (o => new JSR(o('e'), o('a'))),

      "0100 rrr 111 eee aaa" -> (o => new LEA(o('r'), o('e'), o('a'))),
      "1010 xxxxxxxxxxxx"    -> (_ => LINEA),
      "1111 xxxxxxxxxxxx"    -> (_ => LINEF),
      "0100111001010 rrr"    -> (o => new LINK(o('r'))),

      "1110001 d 11 eee aaa; e:2-7" ->
        (o => if o('d') == 0 then new LSRMem(o('e'), o('a')) else new LSLMem(o('e'), o('a'))),
      "1110 ccc d ss i 01 rrr; s:0-2" ->
        (o => if o('d') == 0 then
          addqsize(o) match {
            case 0 => new LSRByteReg(o('c'), o('i'), o('r'))
            case 1 => new LSRShortReg(o('c'), o('i'), o('r'))
            case 2 => new LSRIntReg(o('c'), o('i'), o('r'))
          }
        else
          addqsize(o) match {
            case 0 => new LSLByteReg(o('c'), o('i'), o('r'))
            case 1 => new LSLShortReg(o('c'), o('i'), o('r'))
            case 2 => new LSLIntReg(o('c'), o('i'), o('r'))
          }),

      "00 ss vvv uuu xxx yyy; s:1-3; u:0-7-1" ->
        (o => moveSize(o) match {
          case 0 => new MOVEByte(o('v'), o('u'), o('x'), o('y'))
          case 1 => new MOVEShort(o('v'), o('u'), o('x'), o('y'))
          case 2 => new MOVEInt(o('v'), o('u'), o('x'), o('y'))
        }),
      "00 ss rrr 001 eee aaa; s:2,3" ->
        (o => if isLongChk(o) then new MOVEAInt(o('r'), o('e'), o('a')) else new MOVEAShort(o('r'), o('e'), o('a'))),

      "0100000011 eee aaa; e:0-7-1" -> (o => new MOVEfromSR(o('e'), o('a'))),
      "0100010011 eee aaa; e:0-7-1" -> (o => new MOVEtoCCR(o('e'), o('a'))),
      "0100011011 eee aaa; e:0-7-1" -> (o => new MOVEtoSR(o('e'), o('a'))),
      "0111 rrr 0 dddddddd"         -> (o => new MOVEQ(o('r'), o('d').toByte)),

      "01001 0 001 s eee aaa; e:2,4,5,6" ->
        (o => if isLongMovem(o) then new MOVEMIntToMem(o('e'), o('a')) else new MOVEMShortToMem(o('e'), o('a'))),
      "01001 0 001 s 111 aaa; a:0,1" ->
        (o => if isLongMovem(o) then new MOVEMIntToMem(7, o('a')) else new MOVEMShortToMem(7, o('a'))),
      "01001 1 001 s eee aaa; e:2,3,5,6" ->
        (o =>
          if isLongMovem(o) then new MOVEMIntFromMem(o('e'), o('a'))
          else new MOVEMShortFromMem(o('e'), o('a')),
        ),
      "01001 1 001 s 111 aaa; a:0-3" ->
        (o => if isLongMovem(o) then new MOVEMIntFromMem(7, o('a')) else new MOVEMShortFromMem(7, o('a'))),

      "010011100110 d rrr"        -> (o => new MOVEUSP(o('d'), o('r'))),
      "1100 rrr 111 eee aaa; e:0-7-1" -> (o => new MULS(o('r'), o('e'), o('a'))),
      "1100 rrr 011 eee aaa; e:0-7-1" -> (o => new MULU(o('r'), o('e'), o('a'))),

      "0100100000 eee aaa; e:0-7-1" -> (o => new NBCD(o('e'), o('a'))),
      "01000100 ss eee aaa; s:0-2; e:0-7-1" ->
        (o => addqsize(o) match {
          case 0 => new NEGByte(o('e'), o('a'))
          case 1 => new NEGShort(o('e'), o('a'))
          case 2 => new NEGInt(o('e'), o('a'))
        }),
      "01000000 ss eee aaa; s:0-2; e:0-7-1" ->
        (o => addqsize(o) match {
          case 0 => new NEGXByte(o('e'), o('a'))
          case 1 => new NEGXShort(o('e'), o('a'))
          case 2 => new NEGXInt(o('e'), o('a'))
        }),
      "0100111001110001" -> (_ => NOP),
      "01000110 ss eee aaa; s:0-2; e:0-7-1" ->
        (o => addqsize(o) match {
          case 0 => new NOTByte(o('e'), o('a'))
          case 1 => new NOTShort(o('e'), o('a'))
          case 2 => new NOTInt(o('e'), o('a'))
        }),

      "1000 rrr 0 ss eee aaa; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new ORByteToD(o('r'), o('e'), o('a'))
          case 1 => new ORShortToD(o('r'), o('e'), o('a'))
          case 2 => new ORIntToD(o('r'), o('e'), o('a'))
        }),
      "1000 rrr 1 ss eee aaa; s:0-2; e:2-7" ->
        (o => addqsize(o) match {
          case 0 => new ORByteToEA(o('r'), o('e'), o('a'))
          case 1 => new ORShortToEA(o('r'), o('e'), o('a'))
          case 2 => new ORIntToEA(o('r'), o('e'), o('a'))
        }),
      "00000000 ss eee aaa; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new ORIByte(o('e'), o('a'))
          case 1 => new ORIShort(o('e'), o('a'))
          case 2 => new ORIInt(o('e'), o('a'))
        }),
      "0000000000111100" -> (_ => ORItoCCR),
      "0000000001111100" -> (_ => ORItoSR),

      "0100100001 eee aaa" -> (o => new PEA(o('e'), o('a'))),
      "0100111001110000" -> (_ => RESET),

      "1110011 d 11 eee aaa; e:2-7" ->
        (o => if o('d') == 0 then new RORMem(o('e'), o('a')) else new ROLMem(o('e'), o('a'))),
      "1110 ccc d ss i 11 rrr; s:0-2" ->
        (o => if o('d') == 0 then
          addqsize(o) match {
            case 0 => new RORByteReg(o('c'), o('i'), o('r'))
            case 1 => new RORShortReg(o('c'), o('i'), o('r'))
            case 2 => new RORIntReg(o('c'), o('i'), o('r'))
          }
        else
          addqsize(o) match {
            case 0 => new ROLByteReg(o('c'), o('i'), o('r'))
            case 1 => new ROLShortReg(o('c'), o('i'), o('r'))
            case 2 => new ROLIntReg(o('c'), o('i'), o('r'))
          }),
      "1110010 d 11 eee aaa; e:2-7" ->
        (o => if o('d') == 0 then new ROXRMem(o('e'), o('a')) else new ROXLMem(o('e'), o('a'))),
      "1110 ccc d ss i 10 rrr; s:0-2" ->
        (o => if o('d') == 0 then
          addqsize(o) match {
            case 0 => new ROXRByteReg(o('c'), o('i'), o('r'))
            case 1 => new ROXRShortReg(o('c'), o('i'), o('r'))
            case 2 => new ROXRIntReg(o('c'), o('i'), o('r'))
          }
        else
          addqsize(o) match {
            case 0 => new ROXLByteReg(o('c'), o('i'), o('r'))
            case 1 => new ROXLShortReg(o('c'), o('i'), o('r'))
            case 2 => new ROXLIntReg(o('c'), o('i'), o('r'))
          }),

      "0100111001110011" -> (_ => RTE),
      "0100111001110111" -> (_ => RTR),
      "0100111001110101" -> (_ => RTS),

      "1000 yyy 10000 r xxx" -> (o => new SBCD(o('y'), o('r'), o('x'))),
      "0101 cccc 11 eee aaa; e:0-7-1" -> (o => new Scc(o('c'), o('e'), o('a'))),
      "0100111001110010" -> (_ => STOP),

      "1001 rrr 0 ss eee aaa; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new SUBByteToD(o('r'), o('e'), o('a'))
          case 1 => new SUBShortToD(o('r'), o('e'), o('a'))
          case 2 => new SUBIntToD(o('r'), o('e'), o('a'))
        }),
      "1001 rrr 1 ss eee aaa; s:0-2; e:2-7" ->
        (o => addqsize(o) match {
          case 0 => new SUBByteToEA(o('r'), o('e'), o('a'))
          case 1 => new SUBShortToEA(o('r'), o('e'), o('a'))
          case 2 => new SUBIntToEA(o('r'), o('e'), o('a'))
        }),
      "1001 rrr sss eee aaa; s:3,7" ->
        (o => if isLongA(o) then new SUBAInt(o('r'), o('e'), o('a')) else new SUBAShort(o('r'), o('e'), o('a'))),
      "00000100 ss eee aaa; s:0-2; e:0-7-1" ->
        (o => addqsize(o) match {
          case 0 => new SUBIByte(o('e'), o('a'))
          case 1 => new SUBIShort(o('e'), o('a'))
          case 2 => new SUBIInt(o('e'), o('a'))
        }),
      "0101 ddd 1 ss eee aaa; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new SUBQByte(addqdata(o), o('e'), o('a'))
          case 1 => new SUBQShort(addqdata(o), o('e'), o('a'))
          case 2 => new SUBQInt(addqdata(o), o('e'), o('a'))
        }),
      "1001 yyy 1 ss 00 m xxx; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new SUBXByte(o('x'), o('m'), o('y'))
          case 1 => new SUBXShort(o('x'), o('m'), o('y'))
          case 2 => new SUBXInt(o('x'), o('m'), o('y'))
        }),

      "0100100001000 rrr" -> (o => new SWAP(o('r'))),
      "0100101011 eee aaa" -> (o => new TAS(o('e'), o('a'))),
      "010011100100 vvvv" -> (o => new TRAP(o('v'))),
      "0100111001110110" -> (_ => TRAPV),
      "01001010 ss eee aaa; s:0-2" ->
        (o => addqsize(o) match {
          case 0 => new TSTByte(o('e'), o('a'))
          case 1 => new TSTShort(o('e'), o('a'))
          case 2 => new TSTInt(o('e'), o('a'))
        }),
      "0100111001011 rrr" -> (o => new UNLK(o('r'))),
    ))
    table
  }

  private inline def addqdata(o: Map[Char, Int]): Int = o('d') match {
    case 0 => 8
    case d => d
  }

  /** Return the dispatch table. Read-only after first call. */
  def opcodes: Array[Instruction] = built

  def apply(opcode: Int): Instruction = built(opcode & 0xFFFF)

  // --- Pattern DSL — ported verbatim from the original generator -----------

  private def generate(pattern: String): List[(Int, Map[Char, Int])] = {
    case class Variable(v: Char, seq: collection.Seq[Int], bits: List[Int])

    val Range = "([a-zA-Z]):(?:([0-9]+)-([0-9]+)((?:-[0-9]+)*)|([0-9]+(?:,[0-9]+)*))".r
    val p     = pattern.replace(" ", "").split(";").toIndexedSeq

    require(p.nonEmpty, "empty pattern")
    val bits = p(0)
    require(bits.nonEmpty, "pattern should comprise at least one bit")
    require(
      bits.forall(c => c == '0' || c == '1' || c.isLetter || c == '-'),
      "pattern should comprise only 0's, 1's, letters or -'s",
    )

    val ranges = Map[Char, collection.Seq[Int]](p.drop(1).map {
      case Range(v, lower, upper, null, null)       => v.head -> (lower.toInt to upper.toInt)
      case Range(v, lower, upper, exceptions, null) =>
        val remove = exceptions.split("-").drop(1).map(_.toInt)
        v.head -> (lower.toInt to upper.toInt).filterNot(remove contains _)
      case Range(v, null, null, null, list)         =>
        v.head -> list.split(",").map(_.toInt).toSeq
    }*)

    val (constant, variables) = {
      def scan(acc: Int, pos: Int, chars: List[Char], vars: Map[Char, List[Int]]): (Int, Map[Char, List[Int]]) =
        chars match {
          case Nil                              => (acc, vars)
          case '0' :: t                          => scan(acc, pos << 1, t, vars)
          case '1' :: t                          => scan(acc | pos, pos << 1, t, vars)
          case v :: t if vars contains v         => scan(acc, pos << 1, t, vars + (v -> (vars(v) :+ pos)))
          case v :: t                            => scan(acc, pos << 1, t, vars + (v -> List(pos)))
        }
      scan(0, 1, bits.reverse.toList, Map())
    }

    val out = new ListBuffer[(Int, Map[Char, Int])]

    def enumerate(acc: Int, vars: List[Variable], vals: Map[Char, Int]): Unit = vars match {
      case Nil    => out += ((acc, vals))
      case v :: t => for (i <- v.seq) enumerate(acc | int2bits(0, i, v.bits), t, vals + (v.v -> i))
    }

    def int2bits(res: Int, n: Int, bits: List[Int]): Int = bits match {
      case Nil                   => res
      case b :: t if (n & 1) > 0 => int2bits(res | b, n >> 1, t)
      case _ :: t                => int2bits(res, n >> 1, t)
    }

    enumerate(
      constant,
      variables.toList.map { case (v, b) =>
        if ranges contains v then {
          require(ranges(v).last < (1 << b.length), "second value of range must be less than 2^#bits")
          Variable(v, ranges(v), b)
        } else Variable(v, 0 until (1 << b.length), b)
      },
      Map(),
    )

    out.toList
  }
}
