import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import zio._

object Day8 extends Day[Long, Long] {
  val original: Map[Set[Char], Int] = Map(
    "abcefg".toSet -> 0,
    "cf".toSet -> 1,
    "acdeg".toSet -> 2,
    "acdfg".toSet -> 3,
    "bcdf".toSet -> 4,
    "abdfg".toSet -> 5,
    "abdefg".toSet -> 6,
    "acf".toSet -> 7,
    "abcdefg".toSet -> 8,
    "abcdfg".toSet -> 9
  )

  val baseConstraints = Map(
    'a' -> "abcdefg".toSet,
    'b' -> "abcdefg".toSet,
    'c' -> "abcdefg".toSet,
    'd' -> "abcdefg".toSet,
    'e' -> "abcdefg".toSet,
    'f' -> "abcdefg".toSet,
    'g' -> "abcdefg".toSet
  )

  def reduceConstraints(constraints: Map[Char, Set[Char]], observations: List[String]): Map[Char, Set[Char]] = observations match {
    case Nil => constraints
    case observation :: tail =>
      val observationUnion = observation.map(constraints).foldLeft(Set.empty[Char])(_ union _)
      val possibleNums = original.keySet.filter { chars =>
        chars.size == observation.length &&
          chars.toSet.subsetOf(observationUnion)
      }.flatten
      val reduced = constraints ++ observation.map(c => c -> constraints(c).intersect(possibleNums)).toMap
      reduceConstraints(reduced, tail)
  }

  def bruteForce(chars: List[Char], observations: List[String], mapping: Map[Char, Char]): Option[Map[Char, Char]] = chars match {
    case Nil => Option.when(observations.forall(obs => original.contains(obs.map(mapping).toSet)))(mapping)
    case head :: tail =>
      val alternatives = ("abcdefg".toSet diff mapping.values.toSet)
      LazyList.from(alternatives).map(c => bruteForce(tail, observations, mapping + (head -> c))).collectFirst {
        case Some(a) => a
      }
  }

  case class InputLine(observations: List[String], output: List[String])

  def parseInput(s: String): List[InputLine] = s.split("\n").map { line =>
    val l = line.split("\\|")
    InputLine(l.head.trim.split(" ").toList, l.last.trim.split(" ").toList)
  }.toList

  def part1(in: String) = Task.effect {
    val lines = in.split("\n")
    lines.map(_.split("\\|").last.trim.split(" ").count(x => List(2, 4, 3, 7).contains(x.length))).sum
  }

  def part2(in: String) = Task.effect {
    val input = parseInput(in)
    val x = input.map { line =>
      //      val reduced = reduceConstraints(baseConstraints, line.observations ++ line.output)

      //      if (reduced.exists(_._2.size != 1)) println(s"Not clear! $reduced")
      val maybeMapping = bruteForce("abcdefg".toList, line.observations ++ line.output, Map())
      maybeMapping match {
        case Some(mapping) => line.output.map(s => original(s.map(c => mapping(c)).toSet)).mkString.toInt
        case None =>
          println(s"Couldn't find viable solution for $line")
          0
      }
    }
    x.sum
  }

  val cases = List(
    Test("example", InputString("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")),
    Puzzle(ResourceInput("day8puzzle.txt"))
  )
}
