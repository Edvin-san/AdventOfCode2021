import scala.collection.mutable

object GraphUtil {
  def shortestPath[Vertex, Edge, Cost: Numeric](
                                                 from: Vertex,
                                                 to: Vertex,
                                                 neighborFn: Vertex => Seq[Edge],
                                                 edgeDestinationFn: Edge => Vertex,
                                                 costFn: Edge => Cost)(
                                                 implicit N: Numeric[Cost]
                                               ): Option[(List[Vertex], Cost)] = {
    if (from == to) return Some(Nil, N.zero)
    var previousVertex: Map[Vertex, Vertex] = Map()
    case class PathEnding(prev: Vertex, end: Vertex, pathCost: Cost)
    implicit val ord: Ordering[PathEnding] = (x: PathEnding, y: PathEnding) => N.compare(y.pathCost, x.pathCost)
    val queue: mutable.PriorityQueue[PathEnding] =
      scala.collection.mutable.PriorityQueue[PathEnding](
        neighborFn(from)
          .map(e => PathEnding(from, edgeDestinationFn(e), costFn(e))): _*
      )

    var found = false
    var distance = N.zero
    while (queue.nonEmpty && !found) {
      val pathEnd = queue.dequeue()
      if (!previousVertex.contains(pathEnd.end)) {
        // Found shortest path to this vertex
        previousVertex = previousVertex + (pathEnd.end -> pathEnd.prev)
        if (pathEnd.end == to) {
          found = true
          distance = pathEnd.pathCost
        } else {
          queue.addAll(neighborFn(pathEnd.end).map(e => PathEnding(pathEnd.end, edgeDestinationFn(e), N.plus(costFn(e), pathEnd.pathCost))))
        }
      }
    }

    Option.when(found) {
      // Extract path
      (LazyList.iterate(to)(previousVertex).takeWhile(_ != from).reverse.prepended(from).toList, distance)
    }
  }

  def shortestPathToAll[Vertex, Edge, Cost: Numeric](
                                                      from: Vertex,
                                                      neighborFn: Vertex => Seq[Edge],
                                                      edgeDestinationFn: Edge => Vertex,
                                                      costFn: Edge => Cost)(
                                                      implicit N: Numeric[Cost]
                                                    ): (Map[Vertex, Cost], Map[Vertex, Vertex]) = {
    var previousVertex: Map[Vertex, Vertex] = Map()
    var shortestCostTo: Map[Vertex, Cost] = Map(from -> N.zero)
    case class PathEnding(prev: Vertex, end: Vertex, pathCost: Cost)
    implicit val ord: Ordering[PathEnding] = (x: PathEnding, y: PathEnding) => N.compare(y.pathCost, x.pathCost)
    val queue: mutable.PriorityQueue[PathEnding] =
      scala.collection.mutable.PriorityQueue[PathEnding](
        neighborFn(from)
          .map(e => PathEnding(from, edgeDestinationFn(e), costFn(e))): _*
      )

    while (queue.nonEmpty) {
      val pathEnd = queue.dequeue()
      if (!previousVertex.contains(pathEnd.end)) {
        // Found shortest path to this vertex
        shortestCostTo = shortestCostTo + (pathEnd.end -> pathEnd.pathCost)
        previousVertex = previousVertex + (pathEnd.end -> pathEnd.prev)
        queue.addAll(neighborFn(pathEnd.end).map(e => PathEnding(pathEnd.end, edgeDestinationFn(e), N.plus(costFn(e), pathEnd.pathCost))))
      }
    }

    (shortestCostTo, previousVertex)
  }

}
