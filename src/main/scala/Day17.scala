import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import Util.Vector.{Dir, Pos}
import zio._

object Day17 extends Day[Long, Long] {
  /*
  Tried to do some smart analysis but too difficult.
  Just need to find some bounds for vx, and vy (velocity x, velocity y).
  Also assuming that area is in the 4th quadrant (+x, -y).
  For x, 0 <= vx <= xRange.max (if higher we would overshoot the area on the first step).
  For y, yRange.min <= vy <= -(yRange.min + 1) <=> yRange.min <= vy < -yRange.min,
  if vy < 0, it can at least not be less than yRange.min, because then we would overshoot on first step.
  if vy > 0, we'll end up at y = 0 again with y velocity -(vy + 1), so this can't be less than yRange.min, meaning vy < -yRange.min.

  The highest point for vy >= 0 is vy + (vy - 1) + (vy - 2) + ... + 1 = arithmeticSum(vy) = vy*(vy + 1)/2.
   */

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
        val (u, d) = (yTarget + t*(t+1), 2*t)
        // highest y value is after vy steps, given vy >= 0
        Option.when(u % d == 0)(if (u >= 0) arithmeticSum(u/d) else 0)
      }.toList
    ).max
  }

  def reachesArea(xRange: Range, yRange: Range)(vx: Int, vy: Int): Boolean = {
    def reaches(p: Pos, dir: Dir): Boolean = {
      if (xRange.contains(p.x) && yRange.contains(p.y)) true
      else if (p.x > xRange.max || p.y < yRange.min || dir.x == 0 && p.x < xRange.min) false
      else {
        val xd = if (dir.x > 0) -1 else 0
        reaches(p + dir, dir + Dir(xd, -1))
      }
    }
    reaches(Pos(0, 0), Dir(vx, vy))
  }

  def findValidVelocities(xRange: Range, yRange: Range): List[(Int, Int)] = {
    val reaches = reachesArea(xRange, yRange) _
    (for {
      vy <- yRange.min until -yRange.min
      vx <- 0 to xRange.max
      if reaches(vx, vy)
    } yield vx -> vy).toList
  }

  def arithmeticSum(n: Long): Long = n*(n+1)/2

  case class Range(min: Int, max: Int) {
    def contains(n: Int): Boolean = min <= n && n <= max
  }
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
    findValidVelocities(xR, yR).map { case (_, vy) => if (vy >= 0) arithmeticSum(vy) else 0 }.max
  }

  def part2(in: String) = Task.effect {
    val (xR, yR) = parseInput(in)
    findValidVelocities(xR, yR).size
  }

  val cases = List(
    Test("example", InputString("target area: x=20..30, y=-10..-5"), p1answer = 45, p2answer = 112),
    Puzzle(InputString("target area: x=139..187, y=-148..-89"), p1answer = 10878, p2answer = 4716)
  )
}
