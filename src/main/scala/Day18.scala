import Day18.SnailfishNumber.{Pair, RegularNumber}
import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import Parser._
import Util.LazyListOps
import zio._

object Day18 extends Day[BigInt, BigInt] {
  val doDebug = false

  def debug(s: => String): Unit = if (doDebug) println(s)

  sealed trait SnailfishNumber {
    def prettyPrint: String = {
      def pprint(sn: SnailfishNumber): List[String] = sn match {
        case RegularNumber(value) => List(value.toString)
        case Pair(left, right) =>
          val lpp = pprint(left)
          val rpp = pprint(right)
          val lswidth = lpp.head.size
          val rswidth = rpp.head.size
          val newsub = lpp.tail.zipAll(rpp.tail, " " * lswidth, " " * rswidth).map {
            case (ls, rs) =>
              " " + ls + "   " + rs + " "
          }
          val newsub1 = (lpp.head, rpp.head) match {
            case (ls, rs) =>
              " " + ls + " , " + rs + " "
          }
          ("[" + " " * (newsub1.size - 2) + "]") :: newsub1 :: newsub
      }

      pprint(this).mkString("\n")
    }

    override def toString: String = this match {
      case RegularNumber(value) => value.toString
      case Pair(left, right) => s"[$left,$right]"
    }

    def ++(other: SnailfishNumber): SnailfishNumber = reduceFully(Pair(this, other))
  }

  object SnailfishNumber {
    case class RegularNumber(value: Int) extends SnailfishNumber {
      def +(i: Int): RegularNumber = RegularNumber(value + i)
    }

    case class Pair(left: SnailfishNumber, right: SnailfishNumber) extends SnailfishNumber
  }

  def applyToLeftMostNumber(f: RegularNumber => RegularNumber)(sn: SnailfishNumber): SnailfishNumber = sn match {
    case rn: RegularNumber => f(rn)
    case Pair(left, right) => Pair(applyToLeftMostNumber(f)(left), right)
  }

  def applyToRightMostNumber(f: RegularNumber => RegularNumber)(sn: SnailfishNumber): SnailfishNumber = sn match {
    case rn: RegularNumber => f(rn)
    case Pair(left, right) => Pair(left, applyToRightMostNumber(f)(right))
  }

  case class ReduceResult(sn: SnailfishNumber, left: Option[Int], right: Option[Int], hasExploded: Boolean)

  def reduceExplode(sn: SnailfishNumber, explodeLimit: Int): ReduceResult = sn match {
    case rn: RegularNumber => ReduceResult(rn, None, None, false)
    case pair@Pair(rn1: RegularNumber, rn2: RegularNumber) =>
      if (explodeLimit == 0) ReduceResult(RegularNumber(0), Some(rn1.value).filter(_ > 0), Some(rn2.value).filter(_ > 0), true)
      else ReduceResult(pair, None, None, false)
    case Pair(left, right) =>
      val leftRes = reduceExplode(left, explodeLimit - 1)
      leftRes match {
        case ReduceResult(newLeft, None, None, false) =>
          // Left did not explode
          val rightRes = reduceExplode(right, explodeLimit - 1)
          rightRes match {
            case ReduceResult(newRight, None, None, false) =>
              // Right did not explode
              ReduceResult(Pair(newLeft, newRight), None, None, false)
            case ReduceResult(newRight, maybeLeftAdd, maybeRightAdd, true) =>
              // Right exploded
              val addedLeft = maybeLeftAdd.fold(newLeft)(leftAdd => applyToRightMostNumber(_ + leftAdd)(newLeft))
              ReduceResult(Pair(addedLeft, newRight), None, maybeRightAdd, true)
          }
        case ReduceResult(newLeft, maybeLeftAdd, maybeRightAdd, true) =>
          // Left exploded
          val addedRight = maybeRightAdd.fold(right)(rightAdd => applyToLeftMostNumber(_ + rightAdd)(right))
          ReduceResult(Pair(newLeft, addedRight), maybeLeftAdd, None, true)
      }
  }

  def reduceSplit(sn: SnailfishNumber): (SnailfishNumber, Boolean) = sn match {
    case r@RegularNumber(value) =>
      if (value > 9) Pair(RegularNumber(value / 2), RegularNumber((value + 1) / 2)) -> true
      else r -> false
    case p@Pair(left, right) =>
      reduceSplit(left) match {
        case (newLeft, true) => Pair(newLeft, right) -> true
        case (_, false) => reduceSplit(right) match {
          case (newRight, true) => Pair(left, newRight) -> true
          case (_, false) => p -> false
        }
      }
  }

  def reduceOneStep(sn: SnailfishNumber): SnailfishNumber = {
    val explodeReducedRes = reduceExplode(sn, 4)
    if (explodeReducedRes.sn != sn) {
      debug(s"after explode:  ${explodeReducedRes.sn}, ${explodeReducedRes.left}, ${explodeReducedRes.right}")
      debug(explodeReducedRes.sn.prettyPrint)
      explodeReducedRes.sn
    } else {
      val split = reduceSplit(sn)._1
      if (split != sn) {
        debug(s"after split:    $split")
        debug(split.prettyPrint)
      } else {
        debug("reduced, but nothing happened")
      }
      split
    }
  }

  def reduceFully(sn: SnailfishNumber): SnailfishNumber = {
    debug(s"after addition: $sn")
    debug(sn.prettyPrint)
    LazyList.iterate(sn)(reduceOneStep).getFirstRepeated
  }

  def magnitude(sn: SnailfishNumber): BigInt = sn match {
    case SnailfishNumber.RegularNumber(value) => BigInt(value)
    case SnailfishNumber.Pair(left, right) => magnitude(left) * 3 + magnitude(right) * 2
  }

  val parseRegularNumber: Parser[SnailfishNumber.RegularNumber] = Parser.parseIntOfLen(1).map(RegularNumber.apply)
  val parsePair: Parser[SnailfishNumber.Pair] = for {
    _ <- parseChar('[')
    left <- snailfishNumberParser
    _ <- parseChar(',')
    right <- snailfishNumberParser
    _ <- parseChar(']')
  } yield SnailfishNumber.Pair(left, right)
  val snailfishNumberParser: Parser[SnailfishNumber] = parsePair orElse parseRegularNumber

  def parseInput(s: String): List[SnailfishNumber] = Parser.separatedBy("\n")(snailfishNumberParser).unsafeParse(s)

  def part1(in: String) = Task.effect {
    val numbers = parseInput(in)
    val finalRes = numbers.reduceLeft(_ ++ _)
    magnitude(finalRes)
  }

  def part2(in: String) = Task.effect {
    val numbers = parseInput(in)
    (for {
      n1 <- numbers
      n2 <- numbers
      if n1 != n2
    } yield magnitude(n1 ++ n2)).max
  }

  val cases = List(
    Test("e1", InputString(
      """[1,1]
        |[2,2]
        |[3,3]
        |[4,4]""".stripMargin)),
    Test("e2", InputString(
      """[1,1]
        |[2,2]
        |[3,3]
        |[4,4]
        |[5,5]""".stripMargin)),
    Test("e3", InputString(
      """[1,1]
        |[2,2]
        |[3,3]
        |[4,4]
        |[5,5]
        |[6,6]""".stripMargin)),
    Test("e4", InputString(
      """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
        |[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
        |[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
        |[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
        |[7,[5,[[3,8],[1,4]]]]
        |[[2,[2,2]],[8,[8,1]]]
        |[2,9]
        |[1,[[[9,3],9],[[9,0],[0,7]]]]
        |[[[5,[7,4]],7],1]
        |[[[[4,2],2],6],[8,7]]""".stripMargin)),
    Test("example", InputString(
      """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
        |[[[5,[2,8]],4],[5,[[9,9],0]]]
        |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
        |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
        |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
        |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
        |[[[[5,4],[7,7]],8],[[8,3],8]]
        |[[9,3],[[9,9],[6,[4,9]]]]
        |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
        |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".stripMargin), p1answer = 4140, p2answer = 3993),
    Puzzle(ResourceInput("day18puzzle.txt"), p1answer = 4111, p2answer = 4917)
  )
}
