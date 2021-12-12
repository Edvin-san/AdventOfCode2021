import Day5.LineSegment.PosOps
import Util.Vector.{Dir, Pos}
import zio._
import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
object Day5 extends Day[Long, Long] {
  case class LineSegment(a: Pos, b: Pos) {
    lazy val positions: Set[Pos] = {
      val end = b

      def ps(start: Pos): LazyList[Pos] = if (start == end) LazyList(end) else start #:: ps(start + (b - start).unit)

      ps(a).toSet
    }
  }

  object LineSegment {
    implicit class PosOps(p: Pos) {
      def unit: Dir = Dir(p.x.sign, p.y.sign)
    }
  }

  def intersection(l1: LineSegment, l2: LineSegment): Set[Pos] = l1.positions intersect l2.positions

  def parsePos(s: String): Pos = {
    val parts = s.split(",")
    Pos(parts(0).toInt, parts(1).toInt)
  }

  def parseInput(s: String): List[LineSegment] = s.split("\n").map { l =>
    val parts = l.split(" -> ").map(parsePos)
    LineSegment(parts(0), parts(1))
  }.toList

  def findIntersectingPoints(segments: List[LineSegment]): Set[Pos] = segments match {
    case head :: tail => tail.map(intersection(_, head)).foldLeft(Set.empty[Pos])(_ union _) union findIntersectingPoints(tail)
    case Nil => Set()
  }

  def part1(in: String) = Task.effect {
    findIntersectingPoints(parseInput(in).filter(l => l.a.x == l.b.x || l.a.y == l.b.y)).size
  }

  def part2(in: String) = Task.effect {
    findIntersectingPoints(parseInput(in)).size
  }

  val cases = List(
    Test("example", InputString(
      """0,9 -> 5,9
        |8,0 -> 0,8
        |9,4 -> 3,4
        |2,2 -> 2,1
        |7,0 -> 7,4
        |6,4 -> 2,0
        |0,9 -> 2,9
        |3,4 -> 1,4
        |0,0 -> 8,8
        |5,5 -> 8,2""".stripMargin)),
    Puzzle(ResourceInput("day5puzzle.txt"))
  )
}
