import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import Util.ListOps
import zio._

object Day14 extends Day[Long, Long] {
  case class PolymerGrower(rules: Map[(Char, Char), Char]) {
    def oneStep(current: List[Char]): List[Char] =
      current.sliding(2).flatMap { case List(c1, c2) => if (rules.contains(c1 -> c2)) List(c1, rules(c1 -> c2)) else List(c1) }.toList.appended(current.last)

    def stepSequence(start: List[Char]): LazyList[List[Char]] = start #:: stepSequence(oneStep(start))
  }

  def parseInput(s: String): (String, Map[(Char, Char), Char]) = {
    val Array(template, rulesString) = s.split("\n\n")
    val rules = rulesString.split("\n").map { line =>
      val Array(in, out) = line.split(" -> ")
      (in.head, in.last) -> out.head
    }.toMap
    template -> rules
  }

  def part1(in: String) = Task.effect {
    val (template, rules) = parseInput(in)
    val after10 = PolymerGrower(rules).stepSequence(template.toList)(10)
    val freqs = after10.frequency
    val mostCommon = freqs.values.max
    val leastCommon = freqs.values.min
    mostCommon - leastCommon
  }

  def part2(in: String) = Task.effect {
    ???
  }

  val cases = List(
    Test("example", InputString("""NNCB
                                  |
                                  |CH -> B
                                  |HH -> N
                                  |CB -> H
                                  |NH -> C
                                  |HB -> C
                                  |HC -> B
                                  |HN -> C
                                  |NN -> C
                                  |BH -> H
                                  |NC -> B
                                  |NB -> B
                                  |BN -> B
                                  |BB -> N
                                  |BC -> B
                                  |CC -> N
                                  |CN -> C""".stripMargin), p1answer = 1588),
    Puzzle(ResourceInput("day14puzzle.txt"))
  )
}
