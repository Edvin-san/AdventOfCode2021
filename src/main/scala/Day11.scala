import Util.Vector.Pos
import zio._

object Day11 extends Day[Long, Long] {
  case class EnergyMap(levels: Map[Pos, Int]) {
    lazy val recentFlashes: Int = levels.values.count(_ == 0)
  }

  def neighbors(p: Pos): Set[Pos] = Util.Vector.Dir.all8Dirs.map(p + _).toSet

  // Returns final energy map after all flashes, but not cooled down energy levels.
  def simFlashmageddon(energyLevels: Map[Pos, Int], alreadyFlashed: Set[Pos], flashing: Set[Pos]): EnergyMap =
    if (flashing.isEmpty) EnergyMap(energyLevels)
    else {
      val updatedLevels = for {
        p <- flashing.flatMap(neighbors)
        if energyLevels.contains(p)
        flashingNeighbors = neighbors(p).count(flashing)
      } yield p -> (energyLevels(p) + flashingNeighbors)
      val newFlashing = updatedLevels.collect { case (p, energy) if energy > 9 && !alreadyFlashed(p) && !flashing(p) => p }
      simFlashmageddon(energyLevels ++ updatedLevels.toMap, alreadyFlashed ++ flashing, newFlashing)
    }

  def simulateOneStep(start: EnergyMap): EnergyMap = {
    val l1 = start.levels.mapValuesWith(_ + 1)
    val afterFlashmageddon = simFlashmageddon(l1, Set(), l1.collect { case (p, energy) if energy > 9 => p }.toSet)
    val cooledDown = EnergyMap(afterFlashmageddon.levels.map { case (p, energy) => if (energy > 9) p -> 0 else p -> energy })
    cooledDown
  }

  def stepSequence(start: EnergyMap): LazyList[EnergyMap] = start #:: stepSequence(simulateOneStep(start))

  def parseInput(s: String): EnergyMap = EnergyMap((for {
    (line, y) <- s.split("\n").zipWithIndex.toList
    (c, x) <- line.zipWithIndex
  } yield Pos(x, y) -> c.asDigit).toMap)

  def part1(in: String) = Task.effect {
    val initialEnergyMap = parseInput(in)
    val seq = stepSequence(initialEnergyMap)
    seq.take(101).map(_.recentFlashes).sum
  }

  def part2(in: String) = Task.effect {
    val initialEnergyMap = parseInput(in)
    val seq = stepSequence(initialEnergyMap)

    seq.zipWithIndex.collectFirst { case (energyMap, idx) if energyMap.recentFlashes == 100 => idx }.get
  }

  val inputs = Map(
    "example" -> InputString(
      """5483143223
        |2745854711
        |5264556173
        |6141336146
        |6357385478
        |4167524645
        |2176841721
        |6882881134
        |4846848554
        |5283751526""".stripMargin),
    "puzzle" -> InputString(
      """6111821767
        |1763611615
        |3512683131
        |8582771473
        |8214813874
        |2325823217
        |2222482823
        |5471356782
        |3738671287
        |8675226574""".stripMargin)
  )
}
