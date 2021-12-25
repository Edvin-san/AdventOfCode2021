import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import Util.Vector.Pos
import zio._
import Util._

object Day25 extends Day[Long, Long] {
  sealed trait SeaCucumber

  object SeaCucumber {
    case object EastMoving extends SeaCucumber
    case object SouthMoving extends SeaCucumber
  }
  import SeaCucumber._

  case class SeaFloor(width: Int, height: Int) {
    def nextPos(p: Pos, cucumber: SeaCucumber): Pos = cucumber match {
      case SeaCucumber.EastMoving => Pos((p.x + 1) % width, p.y)
      case SeaCucumber.SouthMoving => Pos(p.x, (p.y + 1) % height)
    }
  }

  def singleStep(seaFloor: SeaFloor)(cucumbers: Map[Pos, SeaCucumber]): Map[Pos, SeaCucumber] = {
    def unoccupied(sit: Map[Pos, SeaCucumber], p: Pos): Boolean = !sit.contains(p)
    val movedEast = cucumbers.map {
      case (p, c: EastMoving.type) =>
        val nextPos = seaFloor.nextPos(p, c)
        if (unoccupied(cucumbers, nextPos)) nextPos -> c
        else p -> c
      case (p, o) => p -> o
    }
    val movedSouth = movedEast.map {
      case (p, c: SouthMoving.type) =>
        val nextPos = seaFloor.nextPos(p, c)
        if (unoccupied(movedEast, nextPos)) nextPos -> c
        else p -> c
      case (p, o) => p -> o
    }
    movedSouth
  }

  def prettyCucumbers(seaFloor: SeaFloor)(cucumbers: Map[Pos, SeaCucumber]): String = {
    (for {
      y <- 0 until seaFloor.height
      line = (for {
        x <- 0 until seaFloor.width
      } yield cucumbers.get(Pos(x, y)).map {
        case SeaCucumber.EastMoving => '>'
        case SeaCucumber.SouthMoving => 'v'
      }.getOrElse('.')).mkString
    } yield line).mkString("\n")
  }

  def part1(in: String) = Task.effect {
    val (seaFloor, initialCucumbers) = parseInput(in)
    val nextStep: Map[Pos, SeaCucumber] => Map[Pos, SeaCucumber] = singleStep(seaFloor)
    val (stable, foundAfterSteps) = LazyList.iterate(initialCucumbers)(nextStep).getFirstRepeatedWithIndex

//    println(prettyCucumbers(seaFloor)(stable))

    foundAfterSteps + 1
  }

  def part2(in: String) = Task.effect {
    ???
  }

  def parseInput(s: String): (SeaFloor, Map[Pos, SeaCucumber]) = {
    val lines = s.split("\n")
    val seaFloor = SeaFloor(lines.head.size, lines.size)
    val cucumbers = for {
      (line, y) <- lines.zipWithIndex
      (c, x) <- line.zipWithIndex
    } yield c match {
      case '.' => None
      case '>' => Some(Pos(x, y) -> EastMoving)
      case 'v' => Some(Pos(x, y) -> SouthMoving)
    }
    seaFloor -> cucumbers.flatten.toMap
  }


  val cases = List(
    Test("example", InputString("""v...>>.vv>
                                  |.vv>>.vv..
                                  |>>.>v>...v
                                  |>>v>>.>.v.
                                  |v>v.vv.v..
                                  |>.>>..v...
                                  |.vv..>.>v.
                                  |v.v..>>v.v
                                  |....v..v.>""".stripMargin), p1answer = 58),
    Puzzle(ResourceInput("day25puzzle.txt"))
  )
}
