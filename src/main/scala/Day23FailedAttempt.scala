import DayCase.{Puzzle, Test}
import Input.InputString
import zio._

object Day23FailedAttempt extends Day[Long, Long] {
  sealed trait Amphipod {
    def moveCost: Int = this match {
      case Amphipod.A => 1
      case Amphipod.B => 10
      case Amphipod.C => 100
      case Amphipod.D => 1000
    }

    override def toString: String = this match {
      case Amphipod.A => "A"
      case Amphipod.B => "B"
      case Amphipod.C => "C"
      case Amphipod.D => "D"
    }
  }

  object Amphipod {
    case object A extends Amphipod

    case object B extends Amphipod

    case object C extends Amphipod

    case object D extends Amphipod
  }

  import Amphipod._

  type NodeId = Int

  case class Node(id: NodeId, isRoom: Boolean, expectedAmphipod: Option[Amphipod]) {
    def isHallway: Boolean = !isRoom
  }

  case class OccupiedNode(node: Node, occupiedBy: Amphipod)

  case class Edge(to: Node, distance: Int)

  def prettyPositions1(positions: Map[Node, Amphipod]): String = {
    val idPos = positions.map(t => t._1.id -> t._2.toString)

    def n(i: Int): String = idPos.getOrElse(i, ".")
    s"""#############
       |#${n(0)}${n(1)}.${n(2)}.${n(3)}.${n(4)}.${n(5)}${n(6)}#
       |###${n(7)}#${n(8)}#${n(9)}#${n(10)}###
       |  #${n(11)}#${n(12)}#${n(13)}#${n(14)}#
       |  #########""".stripMargin
  }

  def prettyPositions2(positions: Map[Node, Amphipod]): String = {
    val idPos = positions.map(t => t._1.id -> t._2.toString)

    def n(i: Int): String = idPos.getOrElse(i, ".")
    s"""#############
       |#${n(0)}${n(1)}.${n(2)}.${n(3)}.${n(4)}.${n(5)}${n(6)}#
       |###${n(7)}#${n(8)}#${n(9)}#${n(10)}###
       |  #${n(11)}#${n(12)}#${n(13)}#${n(14)}#
       |  #${n(15)}#${n(16)}#${n(17)}#${n(18)}#
       |  #${n(19)}#${n(20)}#${n(21)}#${n(22)}#
       |  #########""".stripMargin
  }


  case class AmphipodsSolver(graph: Map[Node, List[Edge]], roomNeighbors: Map[Node, List[Node]]) {
    val nodeById = graph.keys.map(n => n.id -> n).toMap
    private def whereCanPodGo(from: Node, pod: Amphipod, positions: Map[Node, Amphipod]): List[Edge] = {
      def occupied(n: Node): Boolean = positions.contains(n)

      val queue = new scala.collection.mutable.Queue[Edge]
      var visited: Set[NodeId] = Set(from.id)
      var canPotentiallyGoTo = List.empty[Edge]
      val shouldEndAtRoom = from.isHallway
      queue.enqueue(Edge(from, 0))

      while (queue.nonEmpty) {
        val e = queue.dequeue()
        canPotentiallyGoTo = e :: canPotentiallyGoTo
        val at = e.to
        val neighbors = graph(at)
        val canMoveTo = neighbors.filter { neighbor =>
          !(visited(neighbor.to.id) || occupied(neighbor.to)) && !(
            // Can not enter a room with other disgusting amphipods
            at.isHallway &&
              neighbor.to.isRoom &&
              (neighbor.to.expectedAmphipod.get != pod ||
              roomNeighbors(neighbor.to).exists(n => positions.getOrElse(n, pod) != pod))
            )
        }

        visited = visited ++ canMoveTo.map(_.to.id).toSet
        queue.enqueueAll(canMoveTo.map(neighbor => neighbor.copy(distance = e.distance + neighbor.distance)))
      }

      if (shouldEndAtRoom) {
        // If we enter a room, we should go as far as possible
        canPotentiallyGoTo.filter(_.to.isRoom).maxByOption(_.distance).toList
      } else
        canPotentiallyGoTo.filter(_.to.isHallway)
    }

    def organize(initialPositions: Map[Node, Amphipod]): Option[Int] = {
      var bestSoFar = Int.MaxValue
      var cont = true
      def findBestSolution(positions: Map[Node, Amphipod], remainingRooms: Set[Node], totalCost: Int, depth: Int, history: List[Map[Node, Amphipod]]): Unit = {
//        println(s"Searching for solution at depth $depth, totalCost: $totalCost")
//        println(prettyPositions1(positions))

        if (totalCost >= bestSoFar || !cont) ()
        else if (remainingRooms.isEmpty) {
          // Everyone is at a correct position
          bestSoFar = totalCost
          println(s"Found new best solution $bestSoFar at depth $depth")
          println((positions :: history).reverse.map(prettyPositions1).mkString("\n\n"))
          println("HISTORY DONE")
          cont = false
        } else {
          val possibleMoves = for {
            (occupiedNode, pod) <- positions.filter(t => t._1.isHallway || remainingRooms.contains(t._1))
            movesAlongEdge <- whereCanPodGo(occupiedNode, pod, positions)
          } yield occupiedNode -> pod -> movesAlongEdge

          possibleMoves.toList.sortBy(t => (t._2.to.isHallway, -t._1._2.moveCost, t._2.distance*t._1._2.moveCost)).foreach { case ((from, pod), movesAlongEdge) =>
            val newRemaining = if (movesAlongEdge.to.isRoom) remainingRooms - movesAlongEdge.to else remainingRooms
            findBestSolution(
              positions.removed(from).updated(movesAlongEdge.to, pod),
              newRemaining,
              totalCost + movesAlongEdge.distance * pod.moveCost,
              depth + 1,
              positions :: history
            )
          }
        }
      }

      findBestSolution(initialPositions, initialPositions.keySet.filter(_.isRoom), 0, 0, Nil)
      Option.when(bestSoFar != Int.MaxValue)(bestSoFar)
    }
  }

  def part1(in: String) = Task.effect {
    val initialPositions = parseInput(in, unfolded = false)
    val nodeById = p1graph._1.keySet.map(x => x.id -> x).toMap
    val solver = AmphipodsSolver(p1graph._1, p1graph._2)
    solver.organize(initialPositions.map { case (nodeId, pod) => nodeById(nodeId) -> pod }).get
//    ???
  }

  def part2(in: String) = Task.effect {
//    val initialPositions = parseInput(in, unfolded = true)
//    val nodeById = p2graph._1.keySet.map(x => x.id -> x).toMap
//    val solver = AmphipodsSolver(p2graph._1, p2graph._2)
//    val res = solver.organize(initialPositions.map { case (nodeId, pod) => nodeById(nodeId) -> pod })
//    res.get
    ???
  }

  def parseInput(s: String, unfolded: Boolean): Map[NodeId, Amphipod] = {
    val Array(firstRow, secondRow) = s.split("\n").map(_.filter(c => "ABCD".contains(c))).filter(_.nonEmpty)
    val middle = if (unfolded) "DCBADBAC" else ""
    (firstRow ++ middle ++ secondRow).toCharArray.zipWithIndex.map { case (c, idx) =>
      val pod = c match {
        case 'A' => A
        case 'B' => B
        case 'C' => C
        case 'D' => D
      }
      (idx + 7) -> pod
    }.toMap
  }

  val p1graph: (Map[Node, List[Edge]], Map[Node, List[Node]]) = {
    /*
    ##################################
    #0  1     2     3     4      5  6#
    ###### 7  ## 8  ## 9  ## 10 ######
         # 11 ## 12 ## 13 ## 14 #
         ########################
     */
    val nodes = ((0 to 6).map(i => Node(i, false, None)) ++ List(
      Node(7, true, Some(A)),
      Node(8, true, Some(B)),
      Node(9, true, Some(C)),
      Node(10, true, Some(D)),
      Node(11, true, Some(A)),
      Node(12, true, Some(B)),
      Node(13, true, Some(C)),
      Node(14, true, Some(D)),
    )).toVector

    val adjGraph =
      Map(
        nodes(0) -> List(Edge(nodes(1), 1)),
        nodes(1) -> List(Edge(nodes(0), 1), Edge(nodes(2), 2), Edge(nodes(7), 2)),
        nodes(5) -> List(Edge(nodes(4), 2), Edge(nodes(10), 2), Edge(nodes(6), 1)),
        nodes(6) -> List(Edge(nodes(5), 1)),
      ) ++
        (2 to 4).map(i => nodes(i) -> List(nodes(i - 1), nodes(i + 1), nodes(i + 5), nodes(i + 6)).map(Edge(_, 2))).toMap ++
        (7 to 10).map(i => nodes(i) -> List(Edge(nodes(i - 6), 2), Edge(nodes(i - 5), 2), Edge(nodes(i + 4), 1))).toMap ++
        (11 to 14).map(i => nodes(i) -> List(Edge(nodes(i - 4), 1))).toMap

    val roomNeighbors = (7 to 10).map(i => nodes(i) -> List(nodes(i + 4))).toMap
    adjGraph -> roomNeighbors
  }

  val p2graph: (Map[Node, List[Edge]], Map[Node, List[Node]]) = {
    /*
    ##################################
    #0  1     2     3     4      5  6#
    ###### 7  ## 8  ## 9  ## 10 ######
         # 11 ## 12 ## 13 ## 14 #
         # 15 ## 16 ## 17 ## 18 #
         # 19 ## 20 ## 21 ## 22 #
         ########################
     */
    def room(closestToHallway: Int, expected: Amphipod): List[Node] = (0 to 3).map(i => Node(closestToHallway + i * 4, true, Some(expected))).toList

    val nodes = ((0 to 6).map(i => Node(i, false, None)) ++
      room(7, A) ++
      room(8, B) ++
      room(9, C) ++
      room(10, D)).sortBy(_.id).toVector

    val adjGraph =
      Map(
        nodes(0) -> List(Edge(nodes(1), 1)),
        nodes(1) -> List(Edge(nodes(0), 1), Edge(nodes(2), 2), Edge(nodes(7), 2)),
        nodes(5) -> List(Edge(nodes(4), 2), Edge(nodes(10), 2), Edge(nodes(6), 1)),
        nodes(6) -> List(Edge(nodes(5), 1)),
      ) ++
        (2 to 4).map(i => nodes(i) -> List(nodes(i - 1), nodes(i + 1), nodes(i + 5), nodes(i + 6)).map(Edge(_, 2))).toMap ++
        (7 to 10).map(i => nodes(i) -> List(Edge(nodes(i - 6), 2), Edge(nodes(i - 5), 2), Edge(nodes(i + 4), 1))).toMap ++
        (11 to 18).map(i => nodes(i) -> List(Edge(nodes(i - 4), 1), Edge(nodes(i + 4), 1))).toMap ++
        (19 to 22).map(i => nodes(i) -> List(Edge(nodes(i - 4), 1))).toMap

    val roomNeighbors = (7 to 10).map(i => nodes(i) -> List(nodes(i + 4), nodes(i + 8), nodes(i + 12))).toMap
    adjGraph -> roomNeighbors
  }


  val cases = List(
//    Test("example", InputString(
//      """#############
//        |#...........#
//        |###B#C#B#D###
//        |  #A#D#C#A#
//        |  #########""".stripMargin), p1answer = 12521, p2answer = 44169),
    Puzzle(InputString(
      """#############
        |#...........#
        |###D#D#B#A###
        |  #C#A#B#C#
        |  #########""".stripMargin), p1answer = 16508)
  )
}
