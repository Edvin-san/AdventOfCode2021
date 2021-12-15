import Day14.Frequency.CharFrequency
import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import Util.ListOps
import zio._

object Day14 extends Day[BigInt, BigInt] {
  case class PolymerGrower(rules: Map[(Char, Char), Char]) {
    def oneStep(current: List[Char]): List[Char] =
      current.sliding(2).flatMap { case List(c1, c2) => if (rules.contains(c1 -> c2)) List(c1, rules(c1 -> c2)) else List(c1) }.toList.appended(current.last)

    def stepSequence(start: List[Char]): LazyList[List[Char]] = start #:: stepSequence(oneStep(start))
  }

  case class Frequency[A](occurrences: Map[A, BigInt]) extends AnyVal {
    def ++(other: Frequency[A]): Frequency[A] = Frequency(
      (occurrences.toList ++ other.occurrences.toList).groupMapReduce(_._1)(_._2)(_ + _)
    )

    def decremented(c: A): Frequency[A] = Frequency(occurrences.updatedWith(c)(_.map(_ - 1)))

    def decremented(c: A, amount: BigInt): Frequency[A] = Frequency(occurrences.updatedWith(c)(_.map(_ - amount)))

    def max: BigInt = occurrences.values.max

    def min: BigInt = occurrences.values.min
  }

  object Frequency {
    def of[A](c: A, x: Int): Frequency[A] = Frequency(Map(c -> BigInt(x)))

    def ofBig[A](c: A, x: BigInt): Frequency[A] = Frequency(Map(c -> x))

    type CharFrequency = Frequency[Char]
  }

  /**
   * The idea is to use dynamic programming.
   * Characters are never replaced, new characters are introduced between existing ones.
   * So given c1,c2 and c1c2 -> newChar we can figure out independently of other parts of the string what will happen
   * after X generations. Using this we can formulate a recursion
   * freqAfterSteps(c1, c2, steps) = recurse(c1, newChar, steps-1) + recurse(newChar, c1, steps-1) - newChar
   * c1, c2, and steps are probably repeated so we cache the result to speed up the computation.
   */
  case class PolymerFrequencyGrower(rules: Map[(Char, Char), Char]) {
    def frequencyAfterSteps(start: List[Char], steps: Int): CharFrequency = {
      assert(steps >= 0)
      assert(start.size >= 2)

      var cache = Map[(Int, Char, Char), CharFrequency]()

      def freqAfterSteps(c1: Char, c2: Char, remainingSteps: Int): CharFrequency = {
        val id = (remainingSteps, c1, c2)
        if (remainingSteps == 0 || !rules.contains(c1 -> c2)) Frequency.of(c1, 1) ++ Frequency.of(c2, 1)
        else if (cache.contains(id)) cache(id)
        else {
          val midChar = rules(c1 -> c2)
          val f1 = freqAfterSteps(c1, midChar, remainingSteps - 1)
          val f2 = freqAfterSteps(midChar, c2, remainingSteps - 1)
          val combined = (f1 ++ f2).decremented(midChar)
          cache = cache.updated(id, combined)
          combined
        }
      }

      val freqs = start.sliding(2).map(cs => freqAfterSteps(cs.head, cs.last, steps)).toList
      val combinedFreqs = freqs.reduce(_ ++ _)
      start.drop(1).init.foldLeft(combinedFreqs)(_ decremented _)
    }
  }

  case class LowMemoryPolymerFrequencyGrower(rules: Map[(Char, Char), Char]) {
    def frequencyAfterSteps(start: List[Char], steps: Int): CharFrequency = {
      assert(steps >= 0)
      assert(start.size >= 2)

      val alphabet = rules.values.toSet
      var dp: Map[(Char, Char), CharFrequency] = (for {
        c1 <- alphabet
        c2 <- alphabet
      } yield (c1 -> c2) -> (Frequency.of(c1, 1) ++ Frequency.of(c2, 1))).toMap

      var remSteps = steps
      while (remSteps > 0) {
        dp = dp.map { case ((c1, c2), f0) =>
          if (!rules.contains(c1 -> c2)) (c1 -> c2) -> f0
          else {
            val midChar = rules(c1 -> c2)
            val f1 = dp(c1 -> midChar)
            val f2 = dp(midChar -> c2)
            val combined = (f1 ++ f2).decremented(midChar)
            (c1 -> c2) -> combined
          }
        }
        remSteps = remSteps - 1
      }

      val freqs = start.sliding(2).map(cs => dp(cs.head -> cs.last)).toList
      val combinedFreqs = freqs.reduce(_ ++ _)
      start.drop(1).init.foldLeft(combinedFreqs)(_ decremented _)
    }
  }

  /**
   * Didn't come up with this one myself. Idea is to keep track of pair-occurrences.
   * E.g. we know there are X occurrences of ab. ab -> c, so we'll increase the occurrences of ac and cb by count(ab).
   *
   * After looking at Vasyl's solution I understand how to wrap it together.
   * Each character will be counted twice, once in the beginning and once in the end of a pair.
   * Need to add one to the last character tough, because it wasn't counted in the beginning of a pair.
   */
  case class PairWisePolymerFrequencyGrower(rules: Map[(Char, Char), Char]) {
    def frequencyAfterSteps(start: List[Char], steps: Int): CharFrequency = {
      assert(steps >= 0)
      assert(start.size >= 2)

      val pairFreqs: Frequency[(Char, Char)] = start.sliding(2).map(cs => Frequency.of(cs.head -> cs.last, 1)).reduce(_ ++ _)
      val finalPairFreqs = (1 to steps).foldLeft(pairFreqs){
        case (freqs, _) => rules.map { case ((a, b), c) =>
          val abOcc = freqs.occurrences.getOrElse(a -> b, BigInt(0))
          Frequency.ofBig(a -> c, abOcc) ++ Frequency.ofBig(c -> b, abOcc)
        }.reduce(_ ++ _)
      }
      // Extract character frequencies
      // Each character is counted twice for each pair, so for each pair count only the time it is the first char.
      // Add 1 to last char in original string because it is never first in a pair
      Frequency(finalPairFreqs.occurrences.groupMapReduce(_._1._1)(_._2)(_ + _)) ++ Frequency.of(start.last, 1)
    }
  }

  /**
   * Adapted my collegue Vasyl Zhurba's solution.
   */
  case class VasylZhurbaEdition(rules: Map[(Char, Char), Char]) {
    def frequencyAfterSteps(start: List[Char], steps: Int): CharFrequency = {
      assert(steps >= 0)
      assert(start.size >= 2)

      val templateMap: Map[(Char, Char), BigInt] = start.sliding(2).toList.map { case List(x, y) => x -> y -> BigInt(1) }.groupMapReduce(_._1)(_._2)(_ + _)

      val rr: Map[(Char, Char), BigInt] = (1 to steps).foldLeft(templateMap) {
        case (map, _) =>
          map
            .toList
            .flatMap {
              // Note that we assume rules is defined for all pairs, which seems to be the case in input
              case ((x, y), count) => List(x -> rules(x -> y) -> count, rules(x -> y) -> y -> count)
            }.groupMapReduce(_._1)(_._2)(_ + _)
      }

      val grouped1 = (rr + (start.last -> '_' -> BigInt(1))).groupMapReduce(_._1._1)(_._2)(_ + _)
      Frequency(grouped1)
    }
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
    val (template, rules) = parseInput(in)
    val grownFrequency = LowMemoryPolymerFrequencyGrower(rules).frequencyAfterSteps(template.toList, 40)
    grownFrequency.max - grownFrequency.min
  }

  val cases = List(
    Test("example", InputString(
      """NNCB
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
        |CN -> C""".stripMargin), p1answer = 1588, p2answer = 2188189693529L),
    Puzzle(ResourceInput("day14puzzle.txt"), p1answer = 3259, p2answer = 3459174981021L)
  )
}
