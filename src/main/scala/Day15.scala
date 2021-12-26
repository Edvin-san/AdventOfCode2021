import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import Util.Vector.Dir._
import Util.Vector.Pos
import zio._

object Day15 extends Day[Long, Long] {
  def neighborPos1(p: Pos): List[Pos] = List(
    North,
    East,
    South,
    West
  ).map(p + _)

  case class Vertex(p: Pos)

  case class Edge(from: Vertex, to: Vertex, cost: Int)

  case class Graph(vertices: Set[Vertex], edges: Set[Edge])

  def parseInput(s: String): Graph = {
    val pointCost = (for {
      (line, y) <- s.split("\n").zipWithIndex
      (c, x) <- line.toList.zipWithIndex
    } yield Pos(x, y) -> c.asDigit).toMap

    val (maxX, maxY) = (pointCost.keySet.map(_.x).max, pointCost.keySet.map(_.y).max)

    val edges = for {
      y <- 0 to maxY
      x <- 0 to maxX
      p = Pos(x, y)
      neighbor <- neighborPos1(p)
      if pointCost.contains(neighbor)
    } yield Edge(Vertex(p), Vertex(neighbor), pointCost(neighbor))

    Graph(pointCost.keySet.map(Vertex.apply), edges.toSet)
  }

  def shortestPath(graph: Graph, from: Vertex, to: Vertex): Long = {
    val neighbors: Map[Vertex, List[Edge]] = graph.edges.toList.groupBy(_.from)
    GraphUtil.shortestPath[Vertex, Edge, Int](from, to, neighbors.apply, _.to, _.cost).get._2
  }

  def part1(in: String) = Task.effect {
    val graph = parseInput(in)

    val (maxX, maxY) = (graph.vertices.map(_.p.x).max, graph.vertices.map(_.p.y).max)
    val shortest = shortestPath(graph, Vertex(Pos(0, 0)), Vertex(Pos(maxX, maxY)))
    shortest
  }

  // For part 2 needed to expand graph as I was only given 1 out of 25 tiles.
  def expandInput(s: String): String = {
    val lines = s.split("\n")
    val xLen = lines.head.length
    val yLen = lines.size
    val bigLines = for {
      y <- 0 until yLen * 5
      line = (for {
        x <- 0 until xLen * 5
        xmul = x / xLen
        ymul = y / yLen
        orig = lines(y % yLen)(x % xLen).asDigit
        modded = (orig + ymul + xmul) % 10
      } yield if (modded < orig) modded + 1 else modded).mkString
    } yield line
    bigLines.mkString("\n")
  }

  def part2(in: String) = part1(expandInput(in))

  val cases = List(
    Test("example", InputString(
      """1163751742
        |1381373672
        |2136511328
        |3694931569
        |7463417111
        |1319128137
        |1359912421
        |3125421639
        |1293138521
        |2311944581""".stripMargin), p1answer = 40, p2answer = 315),
    Puzzle(ResourceInput("day15puzzle.txt"), p1answer = 462, p2answer = 2846)
  )
}
