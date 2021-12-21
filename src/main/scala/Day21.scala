import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import zio._

object Day21 extends Day[Long, Long] {
  case class PlayerState(at: Int, score: Long)
  case class GameState(p1: PlayerState, p2: PlayerState, turn: Int, die: Int, totalDiceRolls: Int)

  def move(state: GameState): GameState = {
    import state._
    val p = if (turn % 2 == 0) p1 else p2
    val at = (p.at + 3*(die + 1)) % 10
    val score = p.score + at + 1
    if (turn % 2 == 0) state.copy(p1 = PlayerState(at, score), turn = turn + 1, die = die + 3, totalDiceRolls = totalDiceRolls + 3)
    else state.copy(p2 = PlayerState(at, score), turn = turn + 1, die = die + 3, totalDiceRolls = totalDiceRolls + 3)
  }

  def parseInput(s: String): Array[Int] = s.split("\n").map(_.split(": ").last.toInt)

  def part1(in: String) = Task.effect {
    val Array(_1, _2) = parseInput(in)
    val initState = GameState(PlayerState(_1 - 1, 0), PlayerState(_2 - 1, 0), 0, 1, 0)
    val gameProgression = LazyList.iterate(initState)(move)
    val finalState = gameProgression.find(state => state.p1.score >= 1000 || state.p2.score >= 1000).get
    val losingScore = math.min(finalState.p1.score, finalState.p2.score)
    losingScore*finalState.totalDiceRolls
  }

  def part2(in: String) = Task.effect {
    ???
  }

  val cases = List(
    Test("example", InputString("""Player 1 starting position: 4
                                  |Player 2 starting position: 8""".stripMargin), p1answer = 739785),
    Puzzle(InputString("""Player 1 starting position: 8
                         |Player 2 starting position: 3""".stripMargin))
  )
}
