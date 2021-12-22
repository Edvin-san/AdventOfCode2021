import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import zio._

import scala.annotation.tailrec

object Day22 extends Day[Long, Long] {
  case class Point(x: Int, y: Int, z: Int) {
    def +(other: Point): Point = Point(x + other.x, y + other.y, z + other.z)
    def -(other: Point): Point = Point(x - other.x, y - other.y, z - other.z)
    def distTo(other: Point): Double = (other - this).dist
    def dist: Double = math.sqrt(x*x + y*y + z*z)
    def asRelativeTo(origo: Point): Point = this - origo
  }

  // Inclusive
  case class Range(min: Int, max: Int) {
    def contains(n: Int): Boolean = min <= n && n <= max
    def intersect(other: Range): Option[Range] = {
      val List(r1, r2) = List(this, other).sortBy(_.min)
      Option.when(r1.max >= r2.min)(Range(r2.min, math.min(r1.max, r2.max)))
    }
    def values = (min to max)
  }

  object Range {
    val parser: Parser[Range] = Parser.separatedBy("\\.\\.")(Parser.parseInt(10)).map { case List(min, max) => Range(min, max) }
    def parseRange(s: String): Range = {
      val Array(p1, p2) = s.split("\\.\\.")
      Range(p1.drop(2).toInt, p2.toInt)
    }
  }

  case class Cuboid(xs: Range, ys: Range, zs: Range) {
    def intersect(other: Cuboid): Option[Cuboid] = for {
      xr <- xs.intersect(other.xs)
      yr <- ys.intersect(other.ys)
      zr <- zs.intersect(other.zs)
    } yield Cuboid(xr, yr, zr)

    def points = for {
      x <- xs.values
      y <- ys.values
      z <- zs.values
    } yield Point(x, y, z)

    def pointsWithin(otherCube: Cuboid) = intersect(otherCube).map(_.points)
  }

  case class Instruction(on: Boolean, cube: Cuboid)

  object Instruction {
    val parser: Parser[Instruction] = for {
      on <- Parser.eatString("on ").map(_ => true) orElse Parser.eatString("off ").map(_ => false)
      ranges <- Parser.separatedBy(",")(Parser.parseStringOfLen(2) *> Range.parser)
    } yield Instruction(on, Cuboid(ranges.head, ranges(1), ranges(2)))
    def parseInstruction(s: String): Instruction = {
      val Array(onOff, rangeS) = s.split(" ")
      val on = if (onOff == "on") true else false
      val Array(r1, r2, r3) = rangeS.split(",").map(r => Range.parseRange(r))
      Instruction(on, Cuboid(r1, r2, r3))
    }
  }

  def parseInput(s: String): List[Instruction] = s.split("\n").map(Instruction.parseInstruction).toList

  @tailrec
  def cubesOnAfterInitialization(instructions: List[Instruction], alreadyOn: Set[Point], within: Cuboid): Set[Point] = instructions match {
    case head :: tail =>
      val points = head.cube.pointsWithin(within).map(_.toSet).getOrElse(Set.empty)
      val newOn = if (head.on) alreadyOn union points
      else alreadyOn -- points
      cubesOnAfterInitialization(tail, newOn, within)
    case Nil => alreadyOn
  }

  def part1(in: String) = Task.effect {
    val instructions = parseInput(in)
    val within = Cuboid(Range(-50, 50), Range(-50, 50), Range(-50, 50))
    val finalPoints = cubesOnAfterInitialization(instructions, Set.empty, within)
    finalPoints.size
  }

  def part2(in: String) = Task.effect {
    ???
  }

  val cases = List(
    Test("example", InputString("""on x=10..12,y=10..12,z=10..12
                                  |on x=11..13,y=11..13,z=11..13
                                  |off x=9..11,y=9..11,z=9..11
                                  |on x=10..10,y=10..10,z=10..10""".stripMargin), p1answer = 39),
    Test("largeExample", ResourceInput("day22large.txt"), p1answer = 590784),
    Test("p2example", ResourceInput("day22p2example.txt"), p1answer = 474140, p2answer = 2758514936282235L),
    Puzzle(ResourceInput("day22puzzle.txt"), 647062)
  )
}
