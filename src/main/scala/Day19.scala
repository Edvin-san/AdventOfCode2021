import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import Util.ListOps
import zio._

object Day19 extends Day[Long, Long] {
  case class Point(d1: Int, d2: Int, d3: Int) {
    def +(other: Point): Point = Point(d1 + other.d1, d2 + other.d2, d3 + other.d3)
    def -(other: Point): Point = Point(d1 - other.d1, d2 - other.d2, d3 - other.d3)
    def distTo(other: Point): Double = (other - this).dist
    def dist: Double = math.sqrt(d1*d1 + d2*d2 + d3*d3)
    def asRelativeTo(origo: Point): Point = this - origo
  }

  case class ScannerView(points: Set[Point]) {
    def shift(by: Point): ScannerView = ScannerView(points.map(_ + by))

    def merge(other: ScannerView): ScannerView = ScannerView(points ++ other.points)

    lazy val rotations: Vector[Set[Point]] = {
      points.toVector.map { case Point(x, y, z) =>
        Vector(
          // positive x
          Point(+x, +y, +z),
          Point(+x, -z, +y),
          Point(+x, -y, -z),
          Point(+x, +z, -y),

          // negative x
          Point(-x, -y, +z),
          Point(-x, +z, +y),
          Point(-x, +y, -z),
          Point(-x, -z, -y),


          // positive y
          Point(+y, +z, +x),
          Point(+y, -x, +z),
          Point(+y, -z, -x),
          Point(+y, +x, -z),

          // negative y
          Point(-y, -z, +x),
          Point(-y, +x, +z),
          Point(-y, +z, -x),
          Point(-y, -x, -z),

          // positive z
          Point(+z, +x, +y),
          Point(+z, -y, +x),
          Point(+z, -x, -y),
          Point(+z, +y, -x),

          // negative z
          Point(-z, -x, +y),
          Point(-z, +y, +x),
          Point(-z, +x, -y),
          Point(-z, -y, -x),
        )
      }.transpose.map(_.toSet)
    }

    // Potential optimization, compare relative distances to see if could potentially be overlapping
    lazy val allRelativeDistances: Map[Double, Int] = {
      val ps = points.toVector
      (for {
        i1 <- ps.indices
        i2 <- i1 + 1 until ps.size
      } yield ps(i1).distTo(ps(i2))).toList.frequency
    }
    def commonRelativeDistances(other: ScannerView): Int = {
      allRelativeDistances.map { case (d, count) => math.min(count, other.allRelativeDistances.getOrElse(d, 0)) }.sum
    }
  }

  def parseInput(s: String): List[ScannerView] = s.split("\n\n").map(parseScanner).toList

  // Returns scanner 2 position and orientation (relative to scanner 1).
  def overlapping(s1: ScannerView, s2: ScannerView): Option[(Point, ScannerView, Int)] = {
    val s1pointsSize = s1.points.size
    (for {
      (p1, idx) <- LazyList.from(s1.points.zipWithIndex)
      if idx < s1pointsSize - 12
      p1s = s1.points.map(_.asRelativeTo(p1))
      orientation <- LazyList.from(0 to 23)
      p2sOriented = s2.rotations(orientation)
      // Looking for a p2 that is actually the same point as p1
      p2 <- LazyList.from(p2sOriented)
      p2s = p2sOriented.map(_.asRelativeTo(p2))
      intersection = p1s intersect p2s
      if intersection.size >= 12
      shift = p1 - p2
    } yield (shift, ScannerView(p2sOriented), intersection.size)).headOption
  }

  def composeAllScanners(scanners: List[ScannerView]): (ScannerView, Vector[Point]) = {
    val scannerA = scanners.toArray
    def next(bigScanner: ScannerView, unvisited: Set[Int], scannerPositions: Vector[Point]): (ScannerView, Vector[Point]) = {
      if (unvisited.isEmpty) (bigScanner, scannerPositions)
      else {
        val prioUnvisited = unvisited
          .map(idx => idx -> scannerA(idx).commonRelativeDistances(bigScanner))
          .toList
          .filter(_._2 >= 12)
          .sortBy(-_._2)
          .map(_._1)
        val (newMatchIdx, Some((shift, s2, _))) = (for {
          idx <- LazyList.from(prioUnvisited)
          s2 = scannerA(idx)
        } yield idx -> overlapping(bigScanner, s2)).find(_._2.isDefined).get
        next(bigScanner.merge(s2.shift(shift)), unvisited - newMatchIdx, scannerPositions.appended(shift))
      }
    }
    next(scannerA.head, scannerA.indices.toSet - 0, Vector(Point(0, 0, 0)))
  }

  def part1(in: String) = Task.effect {
    val scanners = parseInput(in)
    val (bigScanner, _) = composeAllScanners(scanners)
    bigScanner.points.size
  }

  def part2(in: String) = Task.effect {
    val scanners = parseInput(in)
    val (_, scannerPoints) = composeAllScanners(scanners)
    val manhattanDists = for {
      i1 <- scannerPoints.indices
      i2 <- i1 + 1 until scannerPoints.size
      p1 = scannerPoints(i1)
      p2 = scannerPoints(i2)
      d = p2 - p1
    } yield d.d1.abs + d.d2.abs + d.d3.abs

    manhattanDists.max
  }

  val cases = List(
    Test("example", ResourceInput("day19example1.txt"), p1answer = 79, p2answer = 3621),
    Puzzle(ResourceInput("day19puzzle.txt"), p1answer = 459, p2answer = 19130)
  )

  def parsePoint(s: String): Point = {
    val Array(d1, d2, d3) = s.split(",").map(_.toInt)
    Point(d1, d2, d3)
  }

  def parseScanner(s: String): ScannerView = {
    val lines = s.split("\n")
    val points = lines.tail.map(parsePoint)
    ScannerView(points.toSet)
  }

  // Didn't get orientations correct.
  //  case class Orientation(firstPositive: Boolean, secondPositive: Boolean, thirdPositive: Boolean, axisOrder: List[Axis]) {
  //    val List(a1, a2, a3) = axisOrder
  //
  //    def view(p: Point): Point = {
  //      val m1 = if (firstPositive) 1 else -1
  //      val m2 = if (secondPositive) 1 else -1
  //      val m3 = if (thirdPositive) 1 else -1
  //      Point(a1.choose(p)*m1, a2.choose(p)*m2, a3.choose(p)*m3)
  //    }
  //  }

  //  sealed trait Axis {
  //    def choose(p: Point): Int = this match {
  //      case Axis.X => p.d1
  //      case Axis.Y => p.d2
  //      case Axis.Z => p.d3
  //    }
  //  }
  //
  //  object Axis {
  //    case object X extends Axis
  //    case object Y extends Axis
  //    case object Z extends Axis
  //  }
  //
  //  val axisPermutations: List[List[Axis]] = List(Axis.X, Axis.Y, Axis.Z).permutations.toList
  //  val allOrientations: List[Orientation] = (for {
  //    axisOrder <- axisPermutations
  //    fp <- List(true, false)
  //    sp <- List(true, false)
  //    tp <- List(true, false)
  //    if fp ^ sp ^ tp
  //  } yield Orientation(fp, sp, tp, axisOrder)).toList
}
