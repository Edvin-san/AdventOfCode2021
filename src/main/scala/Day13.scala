import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import Util.Vector.Pos
import zio._

object Day13 extends Day[Long, String] {
  sealed trait Fold

  object Fold {
    case class X(at: Int) extends Fold
    case class Y(at: Int) extends Fold
  }

  case class Paper(points: Set[Pos]) {
    def fold(f: Fold): Paper = f match {
      case Fold.X(at) => Paper(points.map { p =>
        if (p.x > at) Pos(at - (p.x - at), p.y) else p
      })
      case Fold.Y(at) => Paper(points.map { p =>
        if (p.y > at) Pos(p.x, at - (p.y - at)) else p
      })
    }

    override def toString: String = {
      val lines = for {
        y <- 0 to points.maxBy(_.y).y
        xs = 0 to points.maxBy(_.x).x
        line = xs.map(x => if (points(Pos(x, y))) '#' else '.')
      } yield line.mkString
      lines.mkString("\n")
    }
  }

  def parsePos(s: String): Pos = {
    val Array(x, y) = s.split(",").map(_.toInt)
    Pos(x, y)
  }
  def parseInstruction(s: String): Fold = {
    val Array(before, after) = s.split("=")
    val scalar = after.toInt
    before.split(" ").last match {
      case "y" => Fold.Y(scalar)
      case "x" => Fold.X(scalar)
    }
  }
  def parseInput(s: String): (Paper, List[Fold]) = {
    val Array(points, instructions) = s.split("\n\n")
    val positions = points.split("\n").map(parsePos).toSet
    val foldInstructions = instructions.split("\n").map(parseInstruction).toList
    (Paper(positions), foldInstructions)
  }

  def part1(in: String) = Task.effect {
    val (paper, folds) = parseInput(in)
    paper.fold(folds.head).points.size
  }

  def part2(in: String) = Task.effect {
    val (paper, folds) = parseInput(in)
    val folded = folds.foldLeft(paper)(_ fold _)
    println(folded.toString)
    "See printed output"
  }

  val cases = List(
    Test("example", InputString("""6,10
                                  |0,14
                                  |9,10
                                  |0,3
                                  |10,4
                                  |4,11
                                  |6,0
                                  |6,12
                                  |4,1
                                  |0,13
                                  |10,12
                                  |3,4
                                  |3,0
                                  |8,4
                                  |1,10
                                  |2,14
                                  |8,10
                                  |9,0
                                  |
                                  |fold along y=7
                                  |fold along x=5""".stripMargin), p1answer = 17),
    Puzzle(ResourceInput("day13puzzle.txt"))
  )
}
