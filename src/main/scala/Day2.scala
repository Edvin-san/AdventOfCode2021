import Day2.CommandType.{Down, Forward, Up}
import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import zio._

object Day2 extends Day[Int, Int] {
  sealed trait CommandType

  object CommandType {
    case object Forward extends CommandType
    case object Up extends CommandType
    case object Down extends CommandType
  }

  case class Command(tpe: CommandType, scale: Int)

  def parseInput(s: String): List[Command] = s.split("\n").map(_.split(" ").toList match {
    case List(name, x) =>
      val tpe = name match {
        case "forward" => Forward
        case "up" => Up
        case "down" => Down
      }
      Command(tpe, x.toInt)
  }).toList

  def simulate[S](initialState: S, stateUpdate: (S, Command) => S, commands: List[Command]): S = commands match {
    case head :: tail => simulate[S](stateUpdate(initialState, head), stateUpdate, tail)
    case Nil => initialState
  }

  case class P1State(hpos: Int, depth: Int)

  def part1(in: String) = Task.effect {
    val commands = parseInput(in)
    val finalState = simulate[P1State](
      P1State(0, 0),
      {
        case (s, Command(Forward, scale)) => s.copy(hpos = s.hpos + scale)
        case (s, Command(Down, scale)) => s.copy(depth = s.depth + scale)
        case (s, Command(Up, scale)) => s.copy(depth = s.depth - scale)
      },
      commands
    )
    finalState.hpos * finalState.depth
  }

  case class P2State(hpos: Int, depth: Int, aim: Int)

  def part2(in: String) = Task.effect {
    val commands = parseInput(in)
    val finalState = simulate[P2State](
      P2State(0, 0, 0),
      {
        case (s, Command(Forward, scale)) => s.copy(hpos = s.hpos + scale, depth = s.depth + s.aim * scale)
        case (s, Command(Down, scale)) => s.copy(aim = s.aim + scale)
        case (s, Command(Up, scale)) => s.copy(aim = s.aim - scale)
      },
      commands
    )
    finalState.hpos * finalState.depth
  }

  val cases = List(
    Test("example", InputString(
      """forward 5
        |down 5
        |forward 8
        |up 3
        |down 8
        |forward 2""".stripMargin)),
    Puzzle(ResourceInput("day2puzzle.txt"))
  )
}
