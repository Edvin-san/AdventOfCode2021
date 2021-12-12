import zio._
import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
object Day4 extends Day[Int, Int] {
  case class BingoBoard(rows: List[List[Int]]) {
    def allNums: List[Int] = rows.flatten
    override def toString: String = rows.map(_.mkString(" ")).mkString("\n")
  }

  object BingoBoard {
    case class WonBoard(timeToWin: Int, score: Int)
    def winTimeAndScore(board: BingoBoard, bingoNumbers: List[Int]): Option[WonBoard] = {
      val idxNumOccurs: Map[Int, Int] = bingoNumbers.zipWithIndex.toMap // num -> idx
      def whenBingo(row: List[Int]): Option[Int] = {
        val idxCalled = row.flatMap(n => idxNumOccurs.get(n))
        if (idxCalled.size == row.size) Some(idxCalled.max + 1)
        else None
      }

      val minWhenBingo = (board.rows ++ board.rows.transpose).flatMap(whenBingo).minOption
      minWhenBingo.map { bingoTime =>
       val markedNumbers = bingoNumbers.take(bingoTime).toSet
       WonBoard(bingoTime, board.allNums.filterNot(markedNumbers.contains).sum * bingoNumbers(bingoTime - 1))
      }
    }
  }

  def parseInput(s: String): (List[Int], List[BingoBoard]) = {
    val parts = s.split("\n\n")
    val numbers = parts.head.split(",").map(_.toInt).toList
    val boards = parts.tail.map(b => BingoBoard(b.split("\n").map(_.trim.split("\\s+").map(_.toInt).toList).toList)).toList
    numbers -> boards
  }

  def part1(in: String) = Task.effect{
    val (numbers, boards) = parseInput(in)
    boards.flatMap(board => BingoBoard.winTimeAndScore(board, numbers)).minBy(_.timeToWin).score
  }

  def part2(in: String) = Task.effect{
    val (numbers, boards) = parseInput(in)
    boards.flatMap(board => BingoBoard.winTimeAndScore(board, numbers)).maxBy(_.timeToWin).score
  }

  val cases = List(
    Test("example", InputString("""7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
                               |
                               |22 13 17 11  0
                               | 8  2 23  4 24
                               |21  9 14 16  7
                               | 6 10  3 18  5
                               | 1 12 20 15 19
                               |
                               | 3 15  0  2 22
                               | 9 18 13 17  5
                               |19  8  7 25 23
                               |20 11 10 24  4
                               |14 21 16 12  6
                               |
                               |14 21 17 24  4
                               |10 16 15  9 19
                               |18  8 23 26 20
                               |22 11 13  6  5
                               | 2  0 12  3  7""".stripMargin)),
    Puzzle(ResourceInput("day4puzzle.txt"))
  )
}
