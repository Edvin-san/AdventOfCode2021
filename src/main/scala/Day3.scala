import Util.MapOps
import zio._
import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}

object Day3 extends Day[Int, Int] {
  def elemCount[A](xs: List[A]): Map[A, Int] = xs.groupBy(identity).mapValuesWith(_.size)

  def part1(in: String) = Task.effect {
    val bnums = in.split("\n").toList
    val colElemCount = bnums.transpose.map(elemCount)
    val gamma = colElemCount.map(m => m.toList.maxBy(_._2)._1).mkString
    val epsilon = colElemCount.map(m => m.toList.minBy(_._2)._1).mkString
    Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)
  }

  def p2(candidates: List[String], idx: Int, elemToKeep: Map[Char, Int] => Char): String = if (candidates.size == 1) candidates.head else {
    val xs = candidates.map(_ (idx))
    val c = elemToKeep(elemCount(xs))
    p2(candidates.filter(_ (idx) == c), idx + 1, elemToKeep)
  }


  def part2(in: String) = Task.effect {
    val bnums = in.split("\n").toList
    val oxygenNumber = p2(bnums, 0, m => {
      val zeroes = m.getOrElse('0', 0)
      val ones = m.getOrElse('1', 0)
      if (zeroes > ones) '0' else '1'
    })
    val co2scrubberNumber = p2(bnums, 0, m => {
      val zeroes = m.getOrElse('0', 0)
      val ones = m.getOrElse('1', 0)
      if (ones < zeroes) '1' else '0'
    })
    Integer.parseInt(oxygenNumber, 2) * Integer.parseInt(co2scrubberNumber, 2)
  }

  val cases = List(
    Test("example", InputString(
      """00100
        |11110
        |10110
        |10111
        |10101
        |01111
        |00111
        |11100
        |10000
        |11001
        |00010
        |01010""".stripMargin)),
    Puzzle(ResourceInput("day3puzzle.txt"))
  )
}
