import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import zio._

object Day7 extends Day[Long, Long] {
  def parseInput(s: String): Vector[Int] = s.split(",").map(_.toInt).toVector

  def part1(in: String) = Task.effect {
    val crabPositions = parseInput(in)
    val alt = for (i <- crabPositions.min to crabPositions.max) yield
      i -> crabPositions.map(p => (i - p).abs).sum

    alt.minBy(_._2)._2
  }

  def arithmeticSum(n: Int): Int = n * (n + 1) / 2

  def part2(in: String) = Task.effect {
    val crabPositions = parseInput(in)
    val alt = for (i <- crabPositions.min to crabPositions.max) yield
      i -> crabPositions.map(p => arithmeticSum((i - p).abs)).sum

    alt.minBy(_._2)._2
  }

  val cases = List(
    Test("example", InputString("16,1,2,0,4,2,7,1,2,14")),
    Puzzle(ResourceInput("day7puzzle.txt"))
  )
}
