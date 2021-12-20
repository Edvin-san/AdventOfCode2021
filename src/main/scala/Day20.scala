import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import Util.Vector.{Dir, Pos}
import zio._

object Day20 extends Day[Long, Long] {
  def neighborhood(p: Pos): List[Pos] = List(
    Dir.SouthWest,
    Dir.South,
    Dir.SouthEast,
    Dir.West,
    Dir(0, 0),
    Dir.East,
    Dir.NorthWest,
    Dir.North,
    Dir.NorthEast,
    ).map(p + _)

  case class Image(specific: Map[Pos, Int], default: Int) {
    def get(p: Pos): Int = specific.getOrElse(p, default)
    def prettyPrint: String = {
      def intToChar(i: Int): Char = if (i == 1) '#' else '.'
      val (ymin, ymax, xmin, xmax) =
        (specific.keys.map(_.y).min - 3,
          specific.keys.map(_.y).max + 3,
          specific.keys.map(_.x).min - 3,
          specific.keys.map(_.x).max + 3)
      (for {
        y <- ymin to ymax
        line = for {
          x <- xmin to xmax
        } yield intToChar(get(Pos(x, y)))
      } yield line.mkString).mkString("\n")
    }
  }

  case class ImageEnhancer(alg: Vector[Int]) {
    def enhance(img: Image): Image = {
      val pointsToUpdate = img.specific.keySet.flatMap(neighborhood)
      val newSpecific = pointsToUpdate.toList.map(p => p -> alg(Integer.parseInt(neighborhood(p).map(img.get).mkString, 2))).toMap
      val newDefault = alg(Integer.parseInt(img.default.toString*9, 2))
      Image(newSpecific, newDefault)
    }
    def imageSequence(initialImage: Image): LazyList[Image] = LazyList.iterate(initialImage)(enhance)
  }

  def parseInput(s: String): (Vector[Int], Image) = {
    def charToInt(c: Char): Int = if (c == '#') 1 else 0
    val Array(algS, imgS) = s.split("\n\n")
    val alg = algS.map(charToInt).toVector
    val img = for {
      (line, y) <- imgS.split("\n").zipWithIndex
      (c, x) <- line.zipWithIndex
    } yield Pos(x, y) -> charToInt(c)
    alg -> Image(img.toMap, 0)
  }

  def part1(in: String) = Task.effect {
    val (alg, img) = parseInput(in)
    val enhancer = ImageEnhancer(alg)
    enhancer.imageSequence(img)(2).specific.values.sum
  }

  def part2(in: String) = Task.effect {
    val (alg, img) = parseInput(in)
    val enhancer = ImageEnhancer(alg)
    val eh = enhancer.imageSequence(img)(50)
//    println(eh.prettyPrint)
    eh.specific.values.sum
  }

  val cases = List(
    Test("example", InputString("""..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#
                                  |
                                  |#..#.
                                  |#....
                                  |##..#
                                  |..#..
                                  |..###""".stripMargin), p1answer = 35, p2answer = 3351),
    Puzzle(ResourceInput("day20puzzle.txt"), p1answer = 5301, p2answer = 19492)
  )
}
