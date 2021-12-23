import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import zio._

object Day22 extends Day[BigInt, BigInt] {
  case class Point(x: Int, y: Int, z: Int)

  // Inclusive
  case class Range(min: Int, max: Int) {
    def contains(n: Int): Boolean = min <= n && n <= max

    def intersect(other: Range): Option[Range] = {
      val List(r1, r2) = List(this, other).sortBy(_.min)
      Option.when(r1.max >= r2.min)(Range(r2.min, math.min(r1.max, r2.max)))
    }

    def size: Int = max - min + 1

    def values = (min to max)
  }

  object Range {
    val parser: Parser[Range] = Parser.separatedBy("\\.\\.")(Parser.parseInt(10)).map { case List(min, max) => Range(min, max) }

    def parseRange(s: String): Range = {
      val Array(p1, p2) = s.split("\\.\\.")
      Range(p1.drop(2).toInt, p2.toInt)
    }

    def make(from: Int, to: Int): Option[Range] = Option.when(from <= to)(Range(from, to))
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

    def volume: BigInt = List(xs, ys, zs).map(r => BigInt(r.size)).product
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

  def cubesOnAfterInitialization(instructions: List[Instruction], alreadyOn: Set[Point], within: Cuboid): Set[Point] = instructions match {
    case head :: tail =>
      val points = head.cube.pointsWithin(within).map(_.toSet).getOrElse(Set.empty)
      val newOn = if (head.on) alreadyOn union points
      else alreadyOn -- points
      cubesOnAfterInitialization(tail, newOn, within)
    case Nil => alreadyOn
  }

  // PART 2

  case class RangeExcl(inclMin: Int, exclMax: Int) {
    def toRangeIncl: Option[Range] = Range.make(inclMin, exclMax - 1)

    def toSet: Set[Int] = Set(inclMin, exclMax)
  }

  object RangeExcl {
    def fromRangeIncl(r: Range): RangeExcl = RangeExcl(r.min, r.max + 1)
  }


  /**
   * For part 2, we need something smarter. My idea is to represent the current state as a set of non-overlapping "on" cuboids.
   * Each time we turn on/off a cuboid, we make sure the new state has only non-overlapping "on" cuboids. Easier said than done.
   *
   * Some operations that help us:
   * 1. Find intersection of two cuboids
   * 2. Determine if a cuboid completely covers another one
   * 3. Break up a cuboid into several by "removing" a completely covered cuboid from it
   */
  def removeCoveredCuboid(c: Cuboid, covered: Cuboid): Set[Cuboid] = {
    def toRangeExcl(l: List[Int]): RangeExcl = RangeExcl(l.head, l.last)

    def makeNonOverlappingRanges(r1: Range, r2: Range): List[Range] = {
      val (e1, e2) = (RangeExcl.fromRangeIncl(r1), RangeExcl.fromRangeIncl(r2))
      val parts = Set(e1.inclMin, e1.exclMax, e2.inclMin, e2.exclMax)
      val p = parts.toList.sorted
      val res = p.sliding(2).map(toRangeExcl).flatMap(_.toRangeIncl).toList
      res
    }

    val xrs = makeNonOverlappingRanges(c.xs, covered.xs)
    val yrs = makeNonOverlappingRanges(c.ys, covered.ys)
    val zrs = makeNonOverlappingRanges(c.zs, covered.zs)
    (for {
      xr <- xrs
      yr <- yrs
      zr <- zrs
    } yield Cuboid(xr, yr, zr)).toSet - covered
  }

  def rebootStep(cuboidsToProcess: List[Cuboid], resultCuboids: List[Cuboid], instruction: Instruction): Set[Cuboid] = cuboidsToProcess match {
    case Nil =>
      if (instruction.on) resultCuboids.toSet + instruction.cube
      else resultCuboids.toSet
    case c :: remaining => c.intersect(instruction.cube) match {
      case Some(intersectionCube) =>
        if (instruction.on) {
          if (intersectionCube == instruction.cube) // new "on" cube is completely covered
            (cuboidsToProcess ++ resultCuboids).toSet
          else {
            val newParts = removeCoveredCuboid(c, intersectionCube)
            rebootStep(remaining, newParts.toList ++ resultCuboids, instruction)
          }
        } else {
          val newParts = removeCoveredCuboid(c, intersectionCube)
          if (intersectionCube == instruction.cube) // new "off" cube is completely covered
            (remaining ++ resultCuboids).toSet ++ newParts
          else {
            rebootStep(remaining, newParts.toList ++ resultCuboids, instruction)
          }
        }
      case None => rebootStep(remaining, c :: resultCuboids, instruction)
    }

  }

  // Invariant: onCuboids is non-overlapping
  def reboot(instructions: List[Instruction], onCuboids: Set[Cuboid]): Set[Cuboid] = instructions match {
    case Nil => onCuboids
    case instruction :: remainingInstructions =>
      reboot(remainingInstructions, rebootStep(onCuboids.toList, Nil, instruction))
  }

  def part1Original(in: String) = Task.effect {
    val instructions = parseInput(in)
    val within = Cuboid(Range(-50, 50), Range(-50, 50), Range(-50, 50))
    val finalPoints = cubesOnAfterInitialization(instructions, Set.empty, within)
    finalPoints.size
  }

  def part1(in: String) = Task.effect {
    val instructions = parseInput(in)
    val within = Cuboid(Range(-50, 50), Range(-50, 50), Range(-50, 50))
    val finalCuboids = reboot(instructions.filter(_.cube.intersect(within).isDefined), Set.empty)
    finalCuboids.toList.map(_.volume).sum
  }

  def part2(in: String) = Task.effect {
    val instructions = parseInput(in)
    val finalCubes = reboot(instructions, Set.empty)
    val volumes = finalCubes.toList.map(_.volume)
    volumes.sum
  }

  val cases = List(
    Test("example", InputString(
      """on x=10..12,y=10..12,z=10..12
        |on x=11..13,y=11..13,z=11..13
        |off x=9..11,y=9..11,z=9..11
        |on x=10..10,y=10..10,z=10..10""".stripMargin), p1answer = 39, p2answer = 39),
    Test("largeExample", ResourceInput("day22large.txt"), p1answer = 590784, p2answer = 39769202357779L),
    Test("p2example", ResourceInput("day22p2example.txt"), p1answer = 474140, p2answer = 2758514936282235L),
    Puzzle(ResourceInput("day22puzzle.txt"), p1answer = 647062, p2answer = 1319618626668022L)
  )
}
