import Day16.Packet.Literal
import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import zio._

import scala.util.Try

object Day16 extends Day[Long, Long] {
  case class VersionNumber(value: Int) extends AnyVal
  case class TypeId(value: Int) extends AnyVal

  sealed trait Packet {
    def version: Int
  }

  object Packet {
    case class Literal(version: Int, number: Int) extends Packet
    case class Operator(version: Int, typeId: Int, operands: List[Packet]) extends Packet
  }

  trait Parser[+A] {
    def parse(s: String): Option[(A, String)]

    def flatMap[B](f: A => Parser[B]): Parser[B] = ???

    def map[B](f: A => B): Parser[B] = flatMap(a => Parser.succeed(f(a)))
  }

  object Parser {
    def succeed[A](a: A): Parser[A] = new Parser[A] {
      override def parse(s: String): Option[(A, String)] = Some(a -> s)
    }

    def parseInt(len: Int): Parser[Int] = new Parser[Int] {
      override def parse(s: String): Option[(Int, String)] =
        if (s.size >= len) Try(Integer.parseInt(s.take(len), 2)).toOption.map(i => i -> s.drop(len))
        else None
    }

    val versionNumberParser: Parser[VersionNumber] = new Parser[VersionNumber] {
      override def parse(s: String): Option[(VersionNumber, String)] = Option.when(s.length > 3)(
        VersionNumber(s.take(3))
      )
    }

    val literalParser: Parser[Literal] = new Parser[Packet.Literal] {
      override def parse(s: String): Option[(Literal, String)] = {
        val versionNr =
      }
    }
  }


  def part1(in: String) = Task.effect {
    ???
  }

  def part2(in: String) = Task.effect {
    ???
  }

  val cases = List(
    Test("example", InputString("""""")),
    Puzzle(ResourceInput("day16puzzle.txt"))
  )
}
