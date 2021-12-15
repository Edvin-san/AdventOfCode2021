import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import Util.Vector.Dir._
import Util.Vector.Pos
import zio._

import scala.collection.mutable

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

    var shortestEdgeTo: Map[Vertex, Edge] = Map()
    implicit val ord: Ordering[Edge] = (x: Edge, y: Edge) => y.cost - x.cost
    val queue: mutable.PriorityQueue[Edge] = scala.collection.mutable.PriorityQueue[Edge](neighbors(from): _*)

    var found = false
    while (queue.nonEmpty && !found) {
      val e = queue.dequeue()
      if (!shortestEdgeTo.contains(e.to)) {
        // Found shortest path to e.to
        shortestEdgeTo = shortestEdgeTo + (e.to -> e)
        if (e.to == to) {
          found = true
        } else {
          queue.addAll(neighbors(e.to))
        }
      }
    }

    if (!found) throw new NoSuchElementException("Could not find path")

    // Extract path
    def findPath(end: Vertex): LazyList[Edge] =
      if (end == from) LazyList.empty
      else {
        val e = shortestEdgeTo(end)
        e #:: findPath(e.from)
      }

    val path = findPath(to)
    print(path.reverse.toList)
    path.map(_.cost).sum
  }

  def part1(in: String) = Task.effect {
    val graph = parseInput(in)

    val (maxX, maxY) = (graph.vertices.map(_.p.x).max, graph.vertices.map(_.p.y).max)
    val shortest = shortestPath(graph, Vertex(Pos(0, 0)), Vertex(Pos(maxX, maxY)))
    shortest
  }

  def part2(in: String) = Task.effect {
    ???
  }

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
        |2311944581""".stripMargin), p1answer = 40),
    Puzzle(ResourceInput("day15puzzle.txt"))
  )
}
