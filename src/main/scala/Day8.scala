import zio._

object Day8 extends Day[Long, Long] {
  val original: Map[Set[Char], Int] = Map(
    "abcefg".toSet -> 0, // 6
    "cf".toSet -> 1, // 2
    "acdeg".toSet -> 2, // 5
    "acdfg".toSet -> 3, // 5
    "bcdf".toSet -> 4, // 4
    "abdfg".toSet -> 5, // 5
    "abdefg".toSet -> 6, // 6
    "acf".toSet -> 7, // 3
    "abcdefg".toSet -> 8, // 7
    "abcdfg".toSet -> 9 // 6
  )
  val originalNumToChars: Map[Int, Set[Char]] = Map(
    0 -> "abcefg".toSet, // 6
    1 -> "cf".toSet, // 2
    2 -> "acdeg".toSet, // 5
    3 -> "acdfg".toSet, // 5
    4 -> "bcdf".toSet, // 4
    5 -> "abdfg".toSet, // 5
    6 -> "abdefg".toSet, // 6
    7 -> "acf".toSet, // 3
    8 -> "abcdefg".toSet, // 7
    9 -> "abcdfg".toSet // 6
  )

  // Which numbers are part of other numbers, e.g. numParts(0) == Set(1, 7)
  val numParts: Map[Int, Set[Int]] = originalNumToChars.map {
    case (num, chars) => num -> originalNumToChars.filter(_._2.subsetOf(chars)).keySet
  }

  // Size | Possible values
  //   2  | 1 "cf"
  //   3  | 7 "acf"
  //   4  | 4 "bcdf"
  //   5  | 2 "acdeg", 3 "acdfg", 5 "abdfg"
  //   6  | 0 "abcefg", 6 "abdefg", 6 "abcdfg"
  //   7  | 8 "abcdefg"

//  val baseConstraints = Map(
//    'a' -> "abcdefg".toSet,
//    'b' -> "abcdefg".toSet,
//    'c' -> "abcdefg".toSet,
//    'd' -> "abcdefg".toSet,
//    'e' -> "abcdefg".toSet,
//    'f' -> "abcdefg".toSet,
//    'g' -> "abcdefg".toSet
//  )

  def reduceConstraints(constraints: Map[Int, Set[Char]], observations: List[String]): Map[Int, Set[Char]] = observations match {
    case Nil => constraints
    case observation :: tail =>
      val obsSet = observation.toSet
      val possibleNums = originalNumToChars.map { case (num, chars) =>
        val correctSize = chars.size == observation.length
        lazy val doesNotViolateAlreadyDeduced = numParts(num).forall{ i =>
          constraints.getOrElse(i, Set()).subsetOf(obsSet)
        }
        Option.when(correctSize && doesNotViolateAlreadyDeduced)(num)
      }.toList.flatten
      val reduced =
        if (possibleNums.size == 1) constraints + (possibleNums.head -> obsSet)
        else constraints

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

//  def part2(in: String) = Task.effect {
//    val reduced = reduceConstraints(baseConstraints, List("cagedb"))
//    println(reduced.toList.sortBy(_._1).map(t => s"${t._1} -> ${t._2.toList.sorted.mkString}").mkString("\n"))
//    ???
//  }

  def part2(in: String) = Task.effect {
    val input = parseInput(in)
    val x = input.map { line =>
      //      val reduced = reduceConstraints(baseConstraints, line.observations ++ line.output)
      val completelyReduced = LazyList
        .iterate(Map.empty[Int, Set[Char]])(reduceConstraints(_, line.observations))
        .grouped(2)
        .find(it => it.head == it.last)
        .map(_.head)
        .get

      val inverted = completelyReduced.map(t => t._2 -> t._1)

      if (completelyReduced.size != 10) println(s"Not clear! $completelyReduced")
      line.output.map(s => inverted(s.toSet)).mkString.toInt

      //      val maybeMapping = bruteForce("abcdefg".toList, line.observations, Map())
      //      maybeMapping match {
      //        case Some(mapping) => line.output.map(s => original(s.map(c => mapping(c)).toSet)).mkString.toInt
      //        case None =>
      //          println(s"Couldn't find viable solution for $line")
      //          0
      //      }
    }
    x.sum
  }

  val inputs = Map(
    "example" -> InputString("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"),
    "puzzle" -> ResourceInput("day8puzzle.txt")
  )
}
