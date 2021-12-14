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

  case class Frequency[A](occurrences: Map[A, BigInt]) {
    def ++(other: Frequency[A]): Frequency[A] = Frequency(
      (occurrences.keySet ++ other.occurrences.keySet)
        .map(c => c -> (occurrences.getOrElse(c, BigInt(0)) + other.occurrences.getOrElse(c, BigInt(0)))).toMap)

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
   * Need to figure out how to wrap it all together.
   */
  case class PairWisePolymerFrequencyGrower(rules: Map[(Char, Char), Char]) {
    def frequencyAfterSteps(start: List[Char], steps: Int): CharFrequency = {
      assert(steps >= 0)
      assert(start.size >= 2)

      var freqs: Frequency[(Char, Char)] = start.sliding(2).map(cs => Frequency.of(cs.head -> cs.last, 1)).reduce(_ ++ _)

      var remSteps = steps
      while (remSteps > 0) {
        val newFreqs = rules.map { case ((a, b), c) =>
          val abOcc = freqs.occurrences.getOrElse(a -> b, BigInt(0))
          (Frequency.ofBig(a -> c, abOcc) ++ Frequency.ofBig(b -> c, abOcc)).decremented(a -> b, abOcc)
        }.reduce(_ ++ _)
        freqs = Frequency(freqs.occurrences.map { case ((a, b), c) => (a -> b) -> newFreqs.occurrences.getOrElse(a -> b, c) })
        remSteps = remSteps - 1
      }
      // WIP

      /*
      Template:     NNCB
      NN 1, NC 1, CB 1
      After step 1: NCNBCHB
      NC 1, CN 1, NB 1, BC 1, CH 1, HB 1
      After step 2: NBCCNBBBCBHCB
      After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
       */

//      val freqs = start.sliding(2).map(cs => dp(cs.head -> cs.last)).toList
//      val combinedFreqs = freqs.reduce(_ ++ _)
//      start.drop(1).init.foldLeft(combinedFreqs)(_ decremented _)

      ???
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
