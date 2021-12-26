import DayCase.{Puzzle, Test}
import Input.InputString
import zio._

object Day23 extends Day[Long, Long] {
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

    def roomIdx: Int = this match {
      case Amphipod.A => 0
      case Amphipod.B => 1
      case Amphipod.C => 2
      case Amphipod.D => 3
    }
  }

  object Amphipod {
    case object A extends Amphipod
    case object B extends Amphipod
    case object C extends Amphipod
    case object D extends Amphipod
  }

  import Amphipod._

  val roomHallwayIndex = Vector(2, 4, 6, 8)
  case class Room(wanted: Amphipod, content: Vector[Option[Amphipod]]) {
    lazy val isClearOfUnwanted: Boolean = content.forall(_.getOrElse(wanted) == wanted)
    def emptyOuterMost: (Room, Amphipod) = {
      val outerMostIdx = content.indexWhere(_.isDefined)
      this.copy(content = content.updated(outerMostIdx, None)) -> content(outerMostIdx).get
    }
    lazy val emptySpaces: Int = content.count(_.isEmpty)
    def addWantedToRoom: Room = this.copy(content = content.updated(content.lastIndexWhere(_.isEmpty), Some(wanted)))
    def ideal: Room = this.copy(content = content.map(_ => Some(wanted)))
  }
  case class State(hallway: Vector[Option[Amphipod]], rooms: Vector[Room]) {
    def isClear(at: Int): Boolean = hallway(at).isEmpty
    def hallwayCount(a: Int, b: Int): Int = (math.min(a, b) to math.max(a, b)).count(i => !isClear(i))
    def reachableHallwayIndexes(from: Int): Seq[Int] = {
      ((from-1 to 0 by -1).takeWhile(isClear) ++ (from+1 until hallway.size).takeWhile(isClear))
        .filterNot(roomHallwayIndex.contains)
    }

    def possibleNext: Seq[StateWithCost] = {
      // Either someone goes from hallway to their room, or someone leaves their room
      hallway.zipWithIndex.collect {
        case (Some(pod), hallwayIdx)
          if rooms(pod.roomIdx).isClearOfUnwanted &&
            hallwayCount(roomHallwayIndex(pod.roomIdx), hallwayIdx) == 1 =>
          val newHallway = hallway.updated(hallwayIdx, None)
          val newRooms = rooms.updated(pod.roomIdx, rooms(pod.roomIdx).addWantedToRoom)
          val costOfMove = pod.moveCost * (
            math.abs(hallwayIdx - roomHallwayIndex(pod.roomIdx)) + // Move to room
              rooms(pod.roomIdx).emptySpaces // Move into room
            )
          StateWithCost(State(newHallway, newRooms), costOfMove)
      } ++ roomHallwayIndex.zipWithIndex.flatMap {
        case (hallwayIdx, roomIdx) => if (!rooms(roomIdx).isClearOfUnwanted) {
          val (newRoom, pod) = rooms(roomIdx).emptyOuterMost
          reachableHallwayIndexes(hallwayIdx).map { toHallwayIndex =>
            val newHallway = hallway.updated(toHallwayIndex, Some(pod))
            val costOfMove = pod.moveCost * (
              rooms(roomIdx).emptySpaces + 1 + // +1 to go out in hallway
                math.abs(hallwayIdx - toHallwayIndex)
              )
            StateWithCost(State(newHallway, rooms.updated(roomIdx, newRoom)), costOfMove)
          }
        } else Seq.empty
      }
    }

    def prettyPrint: String = {
      def r(i: Int, j: Int): String = rooms(i).content(j).map(_.toString).getOrElse(".")
      s"""#############
         |#${hallway.map(_.map(_.toString).getOrElse(".")).mkString}#
         |###${r(0,0)}#${r(1,0)}#${r(2,0)}#${r(3,0)}###""".stripMargin +
        (1 until rooms.head.content.size).map(i => s"  #${r(0, i)}#${r(1, i)}#${r(2, i)}#${r(3, i)}#").mkString("\n", "\n", "\n") +
      "  #########"
    }
  }
  case class StateWithCost(state: State, cost: Int)

  def leastEnergyPath(from: State, to: State): Option[(List[State], Int)] =
    GraphUtil.shortestPath[State, StateWithCost, Int](
      from,
      to,
      _.possibleNext.toList,
      _.state,
      _.cost
    )

  def part1(in: String) = Task.effect {
    val initialState = parseInput(in, unfolded = false)
    val wantedState = initialState.copy(rooms = initialState.rooms.map(_.ideal))
    val Some((path, cost)) = leastEnergyPath(initialState, wantedState)
//    println(path.zipWithIndex.map { case (state, i) => s"After $i steps: \n${state.prettyPrint}" }.mkString("\n\n"))
    cost
  }

  def part2(in: String) = Task.effect {
    val initialState = parseInput(in, unfolded = true)
    val wantedState = initialState.copy(rooms = initialState.rooms.map(_.ideal))
    val Some((path, cost)) = leastEnergyPath(initialState, wantedState)
//    println(path.zipWithIndex.map { case (state, i) => s"After $i steps: \n${state.prettyPrint}" }.mkString("\n\n"))
    cost
  }

  def parseInput(s: String, unfolded: Boolean): State = {
    val Array(firstRow, secondRow) = s.split("\n").map(_.filter(c => "ABCD".contains(c))).filter(_.nonEmpty)
    val rows = if (unfolded) List(firstRow, "DCBA", "DBAC", secondRow) else List(firstRow, secondRow)
    val rooms = rows.transpose.zip(List(A, B, C, D)).map { case (roomContent, wanted) =>
      Room(wanted,
        roomContent.map {
          case 'A' => A
          case 'B' => B
          case 'C' => C
          case 'D' => D
        }.map(Some(_)).toVector)
    }.toVector
    State(hallway = Vector.fill(11)(None), rooms)
  }

  val cases = List(
    Test("example", InputString(
      """#############
        |#...........#
        |###B#C#B#D###
        |  #A#D#C#A#
        |  #########""".stripMargin), p1answer = 12521, p2answer = 44169),
    Puzzle(InputString(
      """#############
        |#...........#
        |###D#D#B#A###
        |  #C#A#B#C#
        |  #########""".stripMargin), p1answer = 16508, p2answer = 43626)
  )
}
