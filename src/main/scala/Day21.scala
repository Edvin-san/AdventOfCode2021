import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import zio._

object Day21 extends Day[Long, BigInt] {
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

  case class AggregateResult(p1wins: BigInt, p2wins: BigInt) {
    def |+|(other: AggregateResult): AggregateResult = AggregateResult(p1wins + other.p1wins, p2wins + other.p2wins)
  }
  def doctorStrange(p1start: Int, p2start: Int, scoreThreshold: Int): AggregateResult = {
    var memo: Map[(PlayerState, PlayerState, Int, Int), AggregateResult] = Map()
    // p1 always starts
    def dp(p1: PlayerState, p2: PlayerState, remDiceThrows: Int, currentDieSum: Int): AggregateResult = {
      val key = (p1, p2, remDiceThrows, currentDieSum)
      if (memo.contains(key)) memo(key)
      else {
          val res = if (remDiceThrows == 0) {
            val newAt = (p1.at + currentDieSum) % 10
            val newScore = p1.score + newAt + 1
            val newp1 = PlayerState(newAt, newScore)
            if (newp1.score >= scoreThreshold) AggregateResult(1, 0)
            else {
              val reversed = dp(p2, newp1, 3, 0)
              AggregateResult(reversed.p2wins, reversed.p1wins)
            }
          } else {
            (1 to 3).map { die =>
              dp(p1, p2, remDiceThrows - 1, currentDieSum + die)
            }.reduce(_ |+| _)
          }
        memo = memo.updated(key, res)
        res
      }
    }
    dp(PlayerState(p1start, 0), PlayerState(p2start, 0), 3, 0)
  }

  def part2(in: String) = Task.effect {
    val Array(_1, _2) = parseInput(in)
    val res = doctorStrange(_1 - 1, _2 - 1, 21)
    List(res.p1wins, res.p2wins).max
  }

  val cases = List(
    Test("example", InputString("""Player 1 starting position: 4
                                  |Player 2 starting position: 8""".stripMargin), p1answer = 739785, p2answer = 444356092776315L),
    Puzzle(InputString("""Player 1 starting position: 8
                         |Player 2 starting position: 3""".stripMargin), p1answer = 412344, p2answer = 214924284932572L)
  )
}
