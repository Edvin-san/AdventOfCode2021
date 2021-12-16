import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import zio._

import scala.util.{Random, Try}

object Day16 extends Day[Long, BigInt] {
  val doDebug = false
  def debug(s: String) = if (doDebug) println(s)

  case class VersionNumber(value: Int) extends AnyVal

  case class TypeId(value: Int) extends AnyVal

  object TypeId {
    val LiteralTypeId: TypeId = TypeId(4)
  }

  sealed trait Packet {
    def version: VersionNumber
  }

  object Packet {
    case class Literal(version: VersionNumber, number: BigInt) extends Packet

    case class Operator(version: VersionNumber, typeId: TypeId, operands: List[Packet]) extends Packet
  }

  trait Parser[+A] {
    self =>
    def parse(s: String): Option[(A, String)]

    def flatMap[B](f: A => Parser[B]): Parser[B] = new Parser[B] {
      override def parse(s: String): Option[(B, String)] = self.parse(s) match {
        case Some((a, remainder)) =>
          f(a).parse(remainder)
        case None => None
      }
    }

    def map[B](f: A => B): Parser[B] = flatMap(a => Parser.succeed(f(a)))

    def withFilter(f: A => Boolean): Parser[A] = new Parser[A] {
      override def parse(s: String): Option[(A, String)] = self.parse(s) match {
        case Some((a, remainder)) => Option.when(f(a))(a -> remainder)
        case None => None
      }
    }

    def feedInto[B](p: Parser[B])(implicit ev: A <:< String): Parser[B] = new Parser[B] {
      override def parse(s: String): Option[(B, String)] = self.parse(s).flatMap {
        case (a, remainder) => p.parse(a).map { case (b, _) => b -> remainder }
      }
    }
  }

  object Parser {
    def succeed[A](a: A): Parser[A] = new Parser[A] {
      override def parse(s: String): Option[(A, String)] = Some(a -> s)
    }

    def multiple[A](min: Int, max: Option[Int])(p: Parser[A]): Parser[List[A]] = new Parser[List[A]] {
      override def parse(input: String): Option[(List[A], String)] = {
        val id = s"[multiple${Random.nextInt(100)}]"
        debug(s"$id Will try to parse between $min and $max")
        val successfulParseResults = LazyList
          .iterate(p.parse(input))(_.flatMap { case (_, rest) => p.parse(rest) })
          .zipWithIndex
          .takeWhile { case (res, idx) => res.isDefined && max.forall(_ > idx) }
          .tapEach(r => debug(s"$id parsed $r"))
          .map(_._1.get)
          .toList
        debug(s"$id managed to parse ${successfulParseResults.size}, given min = $min and max = $max")
        debug(s"$id full result of parse ${successfulParseResults.map(_._1)}")
        Option.when(successfulParseResults.size >= min) {
          val lastRemainder = successfulParseResults.lastOption.map(_._2).getOrElse(input)
          successfulParseResults.map(_._1) -> lastRemainder
        }
      }
    }

    def parseChar(c: Char): Parser[Char] = new Parser[Char] {
      override def parse(s: String): Option[(Char, String)] = s.headOption.filter(_ == c).map(_ -> s.tail)
    }

    def parseString(len: Int): Parser[String] = new Parser[String] {
      override def parse(s: String): Option[(String, String)] =
        if (s.size >= len) Some(s.take(len) -> s.drop(len))
        else None
    }

    val parseInt: Parser[Int] = new Parser[Int] {
      override def parse(s: String): Option[(Int, String)] = Try(Integer.parseInt(s, 2)).toOption.map(i => i -> "")
    }

    val parseBigInt: Parser[BigInt] = new Parser[BigInt] {
      override def parse(s: String): Option[(BigInt, String)] = Try(BigInt(s, 2)).toOption.map(i => i -> "")
    }

    def parseIntOfLen(len: Int): Parser[Int] = parseString(len).feedInto(parseInt)

    val eatZeroes: Parser[Unit] = (s: String) => Some(() -> s.dropWhile(_ == '0'))
    val versionNumberParser: Parser[VersionNumber] = parseIntOfLen(3).map(VersionNumber.apply)
    val typeIdParser: Parser[TypeId] = parseIntOfLen(3).map(TypeId.apply)

    val literalByte1Parser: Parser[String] = for {
      _ <- parseChar('1')
      s <- parseString(4)
    } yield s

    val literalByteFinalParser: Parser[String] = for {
      _ <- parseChar('0')
      s <- parseString(4)
    } yield s

    def literalPacketParser(versionNumber: VersionNumber): Parser[Packet.Literal] = for {
      _ <- Parser.succeed(debug(s"Beginning to parse a literal $versionNumber"))
      bytes <- multiple(0, None)(literalByte1Parser)
      lastByte <- literalByteFinalParser
      number <- Parser.succeed(bytes.mkString + lastByte).feedInto(parseBigInt)
    } yield Packet.Literal(versionNumber, number)

    def operatorPacketParser(version: VersionNumber, typeId: TypeId): Parser[Packet.Operator] = for {
      _ <- Parser.succeed(debug(s"Beginning to parse an operator $version, $typeId"))
      i <- parseIntOfLen(1)
      subpackets <- i match {
        case 0 => for {
          _ <- Parser.succeed(debug(s"I was 0, so will try to parse bitLen subpackets"))
          bitLen <- parseIntOfLen(15)
          _ = debug(s"bitLen = $bitLen")
          sps <- parseString(bitLen).feedInto(multiple(min = 1, max = None)(packetParser))
        } yield sps
        case 1 => for {
          _ <- Parser.succeed(debug(s"I was 1, so will try to parse numSubpackets subpackets"))
          numSubpackets <- parseIntOfLen(11)
          _ = debug(s"numSubpackets = $numSubpackets")
          sps <- multiple(min = numSubpackets, max = Some(numSubpackets))(packetParser)
        } yield sps
      }
    } yield Packet.Operator(version, typeId, subpackets)

    val packetParser: Parser[Packet] = for {
      version <- versionNumberParser
      _ = debug(s"Parsed $version")
      typeId <- typeIdParser
      _ = debug(s"Parsed $typeId")
      packet <-
        if (typeId == TypeId.LiteralTypeId) literalPacketParser(version)
        else operatorPacketParser(version, typeId)
    } yield packet
  }


  def hexToBinary(hexString: String): String = {
    val unpadded = BigInt(hexString, 16).toString(2)
    val modded = unpadded.size % 4
    val missing = if (modded == 0) 0 else 4 - modded
    unpadded.prependedAll("0"*missing)
  }

  def versionSum(packet: Packet): Int = packet match {
    case Packet.Literal(version, _) => version.value
    case Packet.Operator(version, _, operands) => version.value + operands.map(versionSum).sum
  }

  def compute(packet: Packet): BigInt = packet match {
    case Packet.Literal(_, number) => number
    case Packet.Operator(_, typeId, operands) => typeId.value match {
      case 0 => operands.map(compute).foldLeft(BigInt(0))(_ + _)
      case 1 => operands.map(compute).foldLeft(BigInt(1))(_ * _)
      case 2 => operands.map(compute).min
      case 3 => operands.map(compute).max
      case 5 =>
        val xs = operands.map(compute)
        if (xs.head > xs.last) 1 else 0
      case 6 =>
        val xs = operands.map(compute)
        if (xs.head < xs.last) 1 else 0
      case 7 =>
        val xs = operands.map(compute)
        if (xs.head == xs.last) 1 else 0
    }
  }

  def part1(in: String) = Task.effect {
    val binaryString = hexToBinary(in)
    Parser.packetParser.parse(binaryString) match {
      case Some((packet, _)) => versionSum(packet)
      case None => -1
    }
  }

  def part2(in: String) = Task.effect {
    val binaryString = hexToBinary(in)
    Parser.packetParser.parse(binaryString) match {
      case Some((packet, padding)) => compute(packet)
      case None => BigInt(-1)
    }
  }

  val cases = List(
    Test("literal", InputString("D2FE28"), p1answer = 6),
    Test("operatorLenType0", InputString("38006F45291200"), p1answer = 9),
    Test("operatorLenType1", InputString("EE00D40C823060"), p1answer = 14),
    Test("example1", InputString("8A004A801A8002F478"), p1answer = 16),
    Test("example2", InputString("620080001611562C8802118E34"), p1answer = 12),
    Test("example3", InputString("C0015000016115A2E0802F182340"), p1answer = 23),
    Test("example3", InputString("A0016C880162017C3686B18A3D4780"), p1answer = 31),
    Puzzle(ResourceInput("day16puzzle.txt"), p1answer = 979, 277110354175L)
  )
}
