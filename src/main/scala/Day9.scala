import Util.Vector
import Util.Vector.Dir._
import Util.Vector.Pos
import zio._

object Day9 extends Day[Long, Long] {
  val validDirections: List[Vector.Dir] = List(
    North,
    East,
    South,
    West
  )

  case class HeightMap(height: Map[Pos, Int]) {
    def lowPoints: HeightMap = HeightMap(height.filter { case (p, h) =>
      validDirections.flatMap(dir => height.get(p + dir)).forall(_ > h)
    })
  }

  def basins(hmap: HeightMap): List[Set[Pos]] = {
    def basinExploration(basin: Set[Pos]): LazyList[Set[Pos]] = {
      val newPoints = basin.flatMap(p => validDirections.flatMap { dir =>
        val neighborPos = p + dir
        for {
          neighborHeight <- hmap.height.get(neighborPos)
          if !basin.contains(neighborPos)
          if neighborHeight > hmap.height(p) && neighborHeight < 9
        } yield neighborPos
      })
      basin #:: basinExploration(basin ++ newPoints)
    }

    hmap.lowPoints.height.keySet.toList.map(p => basinExploration(Set(p)).getFirstRepeated)
  }

  def parseInput(s: String): HeightMap = {
    val lines: Array[(String, Int)] = s.split("\n").zipWithIndex
    val heights = (for {
      (line, yIdx) <- lines
      (h, xIdx) <- line.zipWithIndex
    } yield Pos(xIdx, yIdx) -> h.asDigit).toMap
    HeightMap(heights)
  }

  def part1(in: String) = Task.effect {
    val hmap = parseInput(in)
    val low = hmap.lowPoints
    low.height.values.map(_ + 1).sum
  }

  def part2(in: String) = Task.effect {
    val hmap = parseInput(in)
    basins(hmap).map(_.size).sorted.reverse.take(3).product
  }

  val inputs = Map(
    "example" -> InputString(
      """2199943210
        |3987894921
        |9856789892
        |8767896789
        |9899965678""".stripMargin),
    "puzzle" -> ResourceInput("day9puzzle.txt")
  )
}
