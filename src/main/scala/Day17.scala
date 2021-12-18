import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import zio._

object Day17 extends Day[Long, Long] {
  /*
  y: Given Y, find t such that f(t) = Y, where f(t) = t*V - t*(t+1)/2
  t*V - t(t+1)/2 = Y
  t*V - t(t+1)/2 - Y = 0
  t*V = Y + t(t+1)/2
  V = Y/t + (t+1)/2

  =>
  t*V - t(t+1)/2 = Y
  t*(Y + t*(t+1)/(2*V)) - t*(t+1)/2 - Y = 0
  t*Y + t*t*(t+1)/(2*V) - t*(t+1)/2 = Y
  t*Y + t*t*(t+1)/(2*V) - t*(t+1)/2 = Y
  t*Y + (t*t*t)/2V + 1/2V - t*(t+1)/2 = Y

  x: f(t) = t*V - t(t+1)/2, f(t) >= 0
  where does x stop? will spend abs(V) turns to reduce velocity to zero
  x will stop at V + (V - 1) + (V - 2) + ... 2 + 1 = V(V+1)/2
  t = |V|, d = V(V+1)/2

   */

  def findHighestY(xRange: Range, yRange: Range): Long = {
    val tsBasedOnX = (0 to xRange.max).flatMap { vx =>
      if (arithmeticSum(vx) >= xRange.min) {
        val xs = LazyList.iterate(0 -> vx) { case (posX, currvx) => (posX + currvx) -> (currvx - 1) }
        xs.zipWithIndex
          .dropWhile(_._1._1 < xRange.min)
          .takeWhile { case ((posX, currvx), _) => posX <= xRange.max && currvx >= 0 }
          .map(_._2)
      } else LazyList.empty
    }
    tsBasedOnX.toSet.toList.flatMap[Long](t =>
      (yRange.min to yRange.max).flatMap { yTarget =>
        val vy = BigDecimal(yTarget + t*(t+1))/BigDecimal(2*t)
        // highest y value is after vy steps, given vy >= 0
        Option.when(vy.isWhole)(if (vy >= 0) arithmeticSum(vy.toIntExact) else 0)
      }.toList
    ).max
  }

  def arithmeticSum(n: Long): Long = n*(n+1)/2

  case class Range(min: Int, max: Int)
  def parseRange(s: String): Range = {
    val Array(p1, p2) = s.split("\\.\\.")
    Range(p1.drop(2).toInt, p2.toInt)
  }
  def parseInput(s: String): (Range, Range) = {
    val Array(xs, ys) = s.drop(13).split(", ")
    (parseRange(xs), parseRange(ys))
  }

  def part1(in: String) = Task.effect {
    val (xR, yR) = parseInput(in)
    findHighestY(xR, yR)
  }

  def part2(in: String) = Task.effect {
    ???
  }

  val cases = List(
    Test("example", InputString("target area: x=20..30, y=-10..-5")),
//    Puzzle(ResourceInput("dayXpuzzle.txt"))
  )
}
