import Util.MapOps
import zio._

object Day12 extends Day[Long, Long] {
  case class Cave(name: String) {
    lazy val isBig: Boolean = name.toUpperCase == name
    def isSmall: Boolean = !isBig
  }

  object Cave {
    val start = Cave("start")
    val end = Cave("end")
  }

  type CaveGraph = Map[Cave, List[Cave]]

  case class PathFinder(graph: CaveGraph, start: Cave, end: Cave) {
    def pathsToEnd(from: Cave, visitedSmall: Set[Cave]): Long =
      if (from == end) 1
      else {
        val neighbors = graph.getOrElse(from, Nil)
        val candidates = neighbors.filter(cave => cave.isBig || !visitedSmall(cave))
        val newVisited = if (from.isBig) visitedSmall else visitedSmall + from
        candidates.map(pathsToEnd(_, newVisited)).sum
      }

    def pathsToEndAllowingOneSmallRevisit(from: Cave, visitedSmall: Set[Cave]): Long =
      if (from == end) 1
      else {
        val neighbors = graph.getOrElse(from, Nil)
        val candidates = neighbors.filter(cave => cave.isBig || cave != start)
        val newVisited = if (from.isBig) visitedSmall else visitedSmall + from

        candidates.map { cave =>
          if (cave.isSmall && visitedSmall.contains(cave))
            pathsToEnd(cave, newVisited) // Revisit cave
          else
            pathsToEndAllowingOneSmallRevisit(cave, newVisited)
        }.sum
      }

  }

  def parseInput(s: String): CaveGraph = {
    val allPairwiseConn = (for {
      line <- s.split("\n").toList
      caves = line.split("-").map(Cave.apply)
    } yield List(caves(0) -> caves(1), caves(1) -> caves(0))).flatten
    allPairwiseConn.groupBy(_._1).mapValuesWith(_.map(_._2))
  }

  def part1(in: String) = Task.effect {
    val graph = parseInput(in)
    val finder = PathFinder(graph, Cave.start, Cave.end)
    finder.pathsToEnd(Cave.start, Set.empty)
  }

  def part2(in: String) = Task.effect {
    val graph = parseInput(in)
    val finder = PathFinder(graph, Cave.start, Cave.end)
    finder.pathsToEndAllowingOneSmallRevisit(Cave.start, Set.empty)
  }

  val inputs = Map(
    "example1" -> InputString(
      """start-A
        |start-b
        |A-c
        |A-b
        |b-d
        |A-end
        |b-end""".stripMargin),
    "example2" -> InputString(
      """dc-end
        |HN-start
        |start-kj
        |dc-start
        |dc-HN
        |LN-dc
        |HN-end
        |kj-sa
        |kj-HN
        |kj-dc""".stripMargin),
    "example3" -> InputString(
      """fs-end
        |he-DX
        |fs-he
        |start-DX
        |pj-DX
        |end-zg
        |zg-sl
        |zg-pj
        |pj-he
        |RW-he
        |fs-DX
        |pj-RW
        |zg-RW
        |start-pj
        |he-WI
        |zg-he
        |pj-fs
        |start-RW""".stripMargin),
    "puzzle" -> InputString(
      """ln-nr
        |ln-wy
        |fl-XI
        |qc-start
        |qq-wy
        |qc-ln
        |ZD-nr
        |qc-YN
        |XI-wy
        |ln-qq
        |ln-XI
        |YN-start
        |qq-XI
        |nr-XI
        |start-qq
        |qq-qc
        |end-XI
        |qq-YN
        |ln-YN
        |end-wy
        |qc-nr
        |end-nr""".stripMargin)
  )
}
