import Day10.LineStatus.{Corrupted, Incomplete, Ok}
import zio._

object Day10 extends Day[Long, Long] {
  val matching: Map[Char, Char] = "()[]{}<>".grouped(2).flatMap(p => List(p(0) -> p(1), p(1) -> p(0))).toMap

  def isOpening(c: Char): Boolean = "([{<".contains(c)
  def isClosing(c: Char): Boolean = ")]}>".contains(c)

  val score1 = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val score2 = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4
  )


  sealed trait LineStatus

  object LineStatus {
    case object Ok extends LineStatus
    case class Incomplete(opens: List[Char]) extends LineStatus {
      lazy val score: Long = {
        def helper(remainingOpens: List[Char], scoreSoFar: Long): Long = remainingOpens match {
          case head :: tail => helper(tail, scoreSoFar * 5 + score2(matching(head)))
          case Nil => scoreSoFar
        }
        helper(opens, 0)
      }
    }
    case class Corrupted(opens: List[Char], idxInvalid: Int, expected: Option[Char], found: Char) extends LineStatus
  }

  def analyse(line: String): LineStatus = {
    def helper(opens: List[Char], remChars: List[Char], idx: Int): LineStatus = remChars match {
      case Nil => if (opens.isEmpty) Ok else Incomplete(opens)
      case currentChar :: remaining =>
        if (isOpening(currentChar)) {
          helper(currentChar :: opens, remaining, idx + 1)
        } else opens match {
          case Nil => Corrupted(opens, idx, None, currentChar)
          case openingChar :: remOpens =>
            if (matching(openingChar) == currentChar) helper(remOpens, remaining, idx + 1)
            else Corrupted(opens, idx, Some(matching(openingChar)), currentChar)
        }
    }
    helper(Nil, line.toList, 0)
  }

  def part1(in: String) = Task.effect {
    val lineResults = (for {
      line <- in.split("\n")
      status = analyse(line)
    } yield line -> status)

    lineResults.collect {
      case (_, Corrupted(_, _, _, c)) => score1(c)
    }.sum
  }

  def part2(in: String) = Task.effect {
    val lineResults = (for {
      line <- in.split("\n")
      status = analyse(line)
    } yield status)

    val scores = lineResults.collect {
      case a: Incomplete => a.score
    }.toList
    val size = scores.size
    scores.sorted.apply(size / 2)
  }

  val inputs = Map(
    "example" -> InputString("""[({(<(())[]>[[{[]{<()<>>
                               |[(()[<>])]({[<{<<[]>>(
                               |{([(<{}[<>[]}>{[]{[(<()>
                               |(((({<>}<{<{<>}{[]{[]{}
                               |[[<[([]))<([[{}[[()]]]
                               |[{[{({}]{}}([{[{{{}}([]
                               |{<[[]]>}<{[{[{[]{()[[[]
                               |[<(<(<(<{}))><([]([]()
                               |<{([([[(<>()){}]>(<<{{
                               |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin),
    "puzzle" -> ResourceInput("day10puzzle.txt")
  )
}
