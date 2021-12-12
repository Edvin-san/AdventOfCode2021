import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import zio._

object DayX extends Day[Long, Long] {
  def part1(in: String) = Task.effect {
    ???
  }

  def part2(in: String) = Task.effect {
    ???
  }

  val cases = List(
    Test("example", InputString("""""")),
    Puzzle(ResourceInput("dayXpuzzle.txt"))
  )
}
