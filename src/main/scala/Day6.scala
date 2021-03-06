import DayCase.{Puzzle, Test}
import Input.InputString
import Util._
import zio._

object Day6 extends Day[BigInt, BigInt] {
  type Timer = Int

  case class PopulationState(numberOfFishByTimer: Map[Timer, BigInt])

  case class Simulator(newBornTimer: Int, postBirthTimer: Int) {
    def simulatePopulationEvolution(initialState: PopulationState): LazyList[PopulationState] = {
      val nextState = initialState.numberOfFishByTimer.toList.flatMap { case (timer, population) =>
        if (timer > 0)
          List((timer - 1) -> population)
        else
          List(newBornTimer -> population, postBirthTimer -> population)
      }.groupBy(_._1).mapValuesWith(_.map(_._2).sum)
      initialState #:: simulatePopulationEvolution(PopulationState(nextState))
    }
  }

  def parseInput(s: String): PopulationState = PopulationState(s.split(",").map(_.toInt).toList.frequency.mapValuesWith(BigInt(_)))

  def part1(in: String) = Task.effect {
    val simulator = Simulator(newBornTimer = 8, postBirthTimer = 6)
    val daysSimulated = 80
    val initialState = parseInput(in)
    simulator.simulatePopulationEvolution(initialState)(daysSimulated).numberOfFishByTimer.toList.map(_._2).sum
  }

  def part2(in: String) = Task.effect {
    val simulator = Simulator(newBornTimer = 8, postBirthTimer = 6)
    val daysSimulated = 256
    val initialState = parseInput(in)
    simulator.simulatePopulationEvolution(initialState)(daysSimulated).numberOfFishByTimer.toList.map(_._2).sum
  }

  val cases = List(
    Test("example", InputString("3,4,3,1,2")),
    Puzzle(InputString("3,5,4,1,2,1,5,5,1,1,1,1,4,1,4,5,4,5,1,3,1,1,1,4,1,1,3,1,1,5,3,1,1,3,1,3,1,1,1,4,1,2,5,3,1,4,2,3,1,1,2,1,1,1,4,1,1,1,1,2,1,1,1,3,1,1,4,1,4,1,5,1,4,2,1,1,5,4,4,4,1,4,1,1,1,1,3,1,5,1,4,5,3,1,4,1,5,2,2,5,1,3,2,2,5,4,2,3,4,1,2,1,1,2,1,1,5,4,1,1,1,1,3,1,5,4,1,5,1,1,4,3,4,3,1,5,1,1,2,1,1,5,3,1,1,1,1,1,5,1,1,1,1,1,1,1,2,2,5,5,1,2,1,2,1,1,5,1,3,1,5,2,1,4,1,5,3,1,1,1,2,1,3,1,4,4,1,1,5,1,1,4,1,4,2,3,5,2,5,1,3,1,2,1,4,1,1,1,1,2,1,4,1,3,4,1,1,1,1,1,1,1,2,1,5,1,1,1,1,2,3,1,1,2,3,1,1,3,1,1,3,1,3,1,3,3,1,1,2,1,3,2,3,1,1,3,5,1,1,5,5,1,2,1,2,2,1,1,1,5,3,1,1,3,5,1,3,1,5,3,4,2,3,2,1,3,1,1,3,4,2,1,1,3,1,1,1,1,1,1"))
  )
}
