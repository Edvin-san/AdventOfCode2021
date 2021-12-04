import zio._

object Day1 extends Day[Int, Int] {
  def increasing(xs: List[Int]): Int = xs.sliding(2, 1).count(p => p.head < p.last)

  def parseInput(s: String): List[Int] = s.split("\n").map(_.toInt).toList

  def part1(in: String) = Task.effect{
    (parseInput _ andThen increasing)(in)
  }

  def triples(xs: List[Int]): List[Int] = xs.sliding(3, 1).map(_.sum).toList

  def part2(in: String) = Task.effect{
    (parseInput _ andThen triples andThen increasing)(in)
  }

  val inputs = Map(
    "example" -> InputString("""199
                               |200
                               |208
                               |210
                               |200
                               |207
                               |240
                               |269
                               |260
                               |263""".stripMargin),
    "puzzle" -> ResourceInput("day1puzzle.txt")
  )
}
