import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import Parser._
import zio._

object Day16 extends Day[Long, BigInt] {
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

  val versionNumberParser: Parser[VersionNumber] = parseIntOfLen(3, 2).map(VersionNumber.apply)
  val typeIdParser: Parser[TypeId] = parseIntOfLen(3, 2).map(TypeId.apply)

  def literalPacketParser(versionNumber: VersionNumber): Parser[Packet.Literal] = for {
    bytes <- listOf(parseChar('1') *> parseStringOfLen(4))
    lastByte <- parseChar('0') *> parseStringOfLen(4)
    number <- Parser.succeed(bytes.mkString + lastByte).feedInto(parseBigInt(2))
  } yield Packet.Literal(versionNumber, number)

  def operatorPacketParser(version: VersionNumber, typeId: TypeId): Parser[Packet.Operator] = for {
    lengthTypeId <- parseIntOfLen(1, 2)
    subpackets <- lengthTypeId match {
      case 0 => for {
        bitLen <- parseIntOfLen(15, 2)
        sps <- parseStringOfLen(bitLen).feedInto(atLeast(1)(packetParser))
      } yield sps
      case 1 => for {
        numSubpackets <- parseIntOfLen(11, 2)
        sps <- exactly(numSubpackets)(packetParser)
      } yield sps
    }
  } yield Packet.Operator(version, typeId, subpackets)

  val packetParser: Parser[Packet] = for {
    version <- versionNumberParser
    typeId <- typeIdParser
    packet <-
      if (typeId == TypeId.LiteralTypeId) literalPacketParser(version)
      else operatorPacketParser(version, typeId)
  } yield packet

  def hexToBinary(hexString: String): String = {
    val unpadded = BigInt(hexString, 16).toString(2)
    val modded = unpadded.size % 4
    val missing = if (modded == 0) 0 else 4 - modded
    unpadded.prependedAll("0" * missing)
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
    packetParser.parse(binaryString) match {
      case Some((packet, _)) => versionSum(packet)
      case None => -1
    }
  }

  def part2(in: String) = Task.effect {
    val binaryString = hexToBinary(in)
    packetParser.parse(binaryString) match {
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
