import DayCase.{Puzzle, Test}
import Input.{InputString, ResourceInput}
import zio._
import Util._

import scala.util.{Failure, Random, Success, Try}

object Day24 extends Day[Long, Long] {
  sealed trait VarOrValue

  object VarOrValue {
    case class Var(name: Char) extends VarOrValue
    case class Value(value: Int) extends VarOrValue

    def parse(s: String): VarOrValue = Try(s.trim.toInt) match {
      case Failure(_) => Var(s.head)
      case Success(value) => Value(value)
    }
  }

  import VarOrValue._

  sealed trait Instruction

  object Instruction {
    case class Inp(a: Var) extends Instruction
    case class SetVar(a: Var, b: VarOrValue) extends Instruction
    case class Add(a: Var, b: VarOrValue) extends Instruction
    case class Mul(a: Var, b: VarOrValue) extends Instruction
    case class Div(a: Var, b: VarOrValue) extends Instruction
    case class Mod(a: Var, b: VarOrValue) extends Instruction
    case class Eql(a: Var, b: VarOrValue) extends Instruction
    case class NotEql(a: Var, b: VarOrValue) extends Instruction
    case object Noop extends Instruction
  }

  import Instruction._

  case class Memory(vars: Map[Var, Long]) {
    def getValue(b: VarOrValue): Long = b match {
      case v: Var => vars(v)
      case Value(value) => value
    }

    def map(f: Map[Var, Long] => Map[Var, Long]): Memory = Memory(f(vars))

    def pretty: String = "wxyz".map(c => s"$c=${vars.getOrElse(Var(c), 0)}").mkString(", ")
    def w: Long = vars.getOrElse(Var('w'), 0)
    def x: Long = vars.getOrElse(Var('x'), 0)
    def y: Long = vars.getOrElse(Var('y'), 0)
    def z: Long = vars.getOrElse(Var('z'), 0)
  }

  object Memory {
    val init = Memory("wxyz".map(c => Var(c) -> 0L).toMap)
  }

  object Runtime {
    def run(program: List[Instruction], args: List[Int]): Memory = {
      def execute(instructions: List[Instruction], inputs: List[Int], mem: Memory): Memory = instructions match {
        case head :: tail => head match {
          case Inp(a) => execute(tail, inputs.tail, mem.map(_.updated(a, inputs.head)))
          case SetVar(a, b) => execute(tail, inputs, mem.map(_.updated(a, mem.getValue(b))))
          case Add(a, b) => execute(tail, inputs, mem.map(_.updated(a, mem.getValue(a) + mem.getValue(b))))
          case Mul(a, b) => execute(tail, inputs, mem.map(_.updated(a, mem.getValue(a) * mem.getValue(b))))
          case Div(a, b) => execute(tail, inputs, mem.map(_.updated(a, mem.getValue(a) / mem.getValue(b))))
          case Mod(a, b) => execute(tail, inputs, mem.map(_.updated(a, mem.getValue(a) % mem.getValue(b))))
          case Eql(a, b) => execute(tail, inputs, mem.map(_.updated(a, if (mem.getValue(a) == mem.getValue(b)) 1 else 0)))
          case NotEql(a, b) => execute(tail, inputs, mem.map(_.updated(a, if (mem.getValue(a) != mem.getValue(b)) 1 else 0)))
          case Noop => execute(tail, inputs, mem)
        }
        case Nil => mem
      }

      execute(program, args, Memory.init)
    }

    val configs = List(
      (7, false, 0),
      (8, false, 0),
      (2, false, 0),
      (11, false, 0),
      (6, true, 3),
      (12, false, 0),
      (14, false, 0),
      (13, true, 16),
      (15, false, 0),
      (10, true, 8),
      (6, true, 12),
      (10, true, 7),
      (8, true, 6),
      (5, true, 11),
    )

    def runSuperOptimized(args: List[Int]): Long = {
      def run(argsWithConfig: List[(Int, (Int, Boolean, Int))], prevZ: Long): Long = argsWithConfig match {
        case Nil => prevZ
        case (d, (yc, doBranch, digitDiff)) :: tail =>
          if (doBranch) {
           if (prevZ % 26 - digitDiff == d) run(tail, prevZ / 26)
           else run(tail, (prevZ / 26) * 26 + d + yc)
          } else {
            run(tail, prevZ*26 + d + yc)
          }
      }
      run(args.zip(configs), 0)
    }

    def findMaxValidModelNumber: Option[Long] = {
      var memo: Map[(Int, Long), Option[Long]] = Map.empty
      def findMaxNumResultingInZ(z: Long, conf: List[(Int, Boolean, Int)]): Option[Long] = conf match {
        case Nil => Some(0)
        case (yc, doBranch, digitDiff) :: tail =>
          val key = conf.size -> z
          if (memo.contains(key)) memo(key)
          else {
            val best = if (!doBranch) {
              (1 to 9).flatMap { digit =>
                val zz = z - digit - yc
                if (zz % 26 != 0) None
                else findMaxNumResultingInZ(zz / 26, tail).map(l => l*10 + digit)
              }.maxOption
            } else {
              // 1, digit match so x = 0
              val case1 = (1 to 9).flatMap { digit =>
                findMaxNumResultingInZ(z*26 + digit + digitDiff, tail).map(l => l*10 + digit)
              }
              // 2. no digit match, so x <- 1
              val case2 = (1 to 9).flatMap { digit =>
                val zz = z - digit - yc
                if (zz % 26 != 0) Nil
                else (0 to 25)
                  .filterNot(extra => extra == digit + digitDiff) // Need to avoid case 1
                  .map(extra => zz + extra)
                  .flatMap(zPrev => findMaxNumResultingInZ(zPrev, tail).map(l => l*10 + digit))
              }
              (case1 ++ case2).maxOption
            }
            memo = memo.updated(key, best)
            best
          }
      }
      val res = findMaxNumResultingInZ(0, configs.reverse)
      println(s"After running max search: memo size ${memo.size}, where defined: ${memo.values.flatten.size}")
      res
    }

    def findMinValidModelNumber: Option[Long] = {
      var memo: Map[(Int, Long), Option[Long]] = Map.empty
      def findMinNumResultingInZ(z: Long, conf: List[(Int, Boolean, Int)]): Option[Long] = conf match {
        case Nil => Some(0)
        case (yc, doBranch, digitDiff) :: tail =>
          val key = conf.size -> z
          if (memo.contains(key)) memo(key)
          else {
            val best = if (!doBranch) {
              (1 to 9).flatMap { digit =>
                val zz = z - digit - yc
                if (zz % 26 != 0) None
                else findMinNumResultingInZ(zz / 26, tail).map(l => l*10 + digit)
              }.minOption
            } else {
              // 1, digit match so x = 0
              val case1 = (1 to 9).flatMap { digit =>
                findMinNumResultingInZ(z*26 + digit + digitDiff, tail).map(l => l*10 + digit)
              }
              // 2. no digit match, so x <- 1
              val case2 = (1 to 9).flatMap { digit =>
                val zz = z - digit - yc
                if (zz % 26 != 0) Nil
                else (0 to 25)
                  .filterNot(extra => extra == digit + digitDiff) // Need to avoid case 1
                  .map(extra => zz + extra)
                  .flatMap(zPrev => findMinNumResultingInZ(zPrev, tail).map(l => l*10 + digit))
              }
              (case1 ++ case2).minOption
            }
            memo = memo.updated(key, best)
            best
          }
      }
      val res = findMinNumResultingInZ(0, configs.reverse)
      println(s"After running min search: memo size ${memo.size}, where defined: ${memo.values.flatten.size}")
      res
    }


  }

  def optimize(program: List[Instruction]): List[Instruction] = {
    def transformPairs(instrs: List[Instruction], f: List[Instruction] => List[Instruction]): List[Instruction] = {
      val g1 = instrs.grouped(2).flatMap(f).toList
      g1.head :: g1.tail.grouped(2).flatMap(f).toList
    }
    def removeMul0Add(instrs: List[Instruction]): List[Instruction] = transformPairs(instrs, {
      case List(Mul(x, Value(0)), Add(y, v)) if x == y => List(SetVar(x, v))
      case l => l
    })

    def makeNotEql(instrs: List[Instruction]): List[Instruction] = transformPairs(instrs, {
      case List(Eql(x, y), Eql(x1, Value(0))) if x == x1 => List(NotEql(x, y))
      case l => l
    })

    val withoutUselessDiv = program.filterNot {
      case Div(_, Value(1)) => true
      case _ => false
    }

    val withSet = LazyList.iterate(withoutUselessDiv)(removeMul0Add).getFirstRepeated
    val withNotEql = makeNotEql(withSet)
    withNotEql
  }

  def parseInput(s: String): List[Instruction] = {
    (for {
      line <- s.split("\n")
      instr = line.split(" ") match {
        case Array("inp", c) => Inp(Var(c.head))
        case Array("add", a, b) => Add(Var(a.head), VarOrValue.parse(b))
        case Array("mul", a, b) => Mul(Var(a.head), VarOrValue.parse(b))
        case Array("div", a, b) => Div(Var(a.head), VarOrValue.parse(b))
        case Array("mod", a, b) => Mod(Var(a.head), VarOrValue.parse(b))
        case Array("eql", a, b) => Eql(Var(a.head), VarOrValue.parse(b))
      }
    } yield instr).toList
  }

  def part1brute(in: String) = for {
    program <- Task(parseInput(in))
    xs = List((11111111111111L, 12342734883266L), (12345678912345L, 15342759967696L), (15345678912345L, 17342734954328L), (17345678912345L, 99996647259948L)).map {
      case (min, max) => ZIO.succeed(LazyList
        .iterate(max)(_ - 1)
        .filterNot(_.toString.contains('0'))
        .takeWhile(_ >= min)
        .find(l => {
          val runRes = Runtime.run(program, l.toString.map(_.asDigit).toList).vars
          println(s"For $l: w=${runRes(Var('w'))}, x=${runRes(Var('x'))}, y=${runRes(Var('y'))}, z=${runRes(Var('z'))}")
          runRes(Var('z')) == 0
        })).someOrFailException
    }
    found <- ZIO.raceAll(xs.head, xs.tail)
  } yield found

  def prettyVarOrValue(b: VarOrValue): String = b match {
    case Var(name) => name.toString
    case Value(value) => value.toString
  }
  def prettyProgram(program: List[Instruction]): String = program.map {
    case Inp(a) => s"inp ${a.name}"
    case SetVar(a, b) => s"${a.name} <- ${prettyVarOrValue(b)}"
    case Add(a, b) => s"${a.name} += ${prettyVarOrValue(b)}"
    case Mul(a, b) => s"${a.name} *= ${prettyVarOrValue(b)}"
    case Div(a, b) => s"${a.name} /= ${prettyVarOrValue(b)}"
    case Mod(a, b) => s"${a.name} = ${a.name} % ${prettyVarOrValue(b)}"
    case Eql(a, b) => s"${a.name} <- if (${a.name} == ${prettyVarOrValue(b)}) 1 else 0"
    case NotEql(a, b) => s"${a.name} <- if (${a.name} != ${prettyVarOrValue(b)}) 1 else 0"
    case Instruction.Noop => "noop"
  }.mkString("\n")

  def samplesInputsBetween(n: Int)(min: Long, max: Long): List[List[Int]] = {
    val ns = (min to max).filterNot(_.toString.contains('0'))
    Random.shuffle(ns).take(n).map(_.toString.map(_.asDigit).toList).toList
  }

  def part1(in: String) = Task.effect {

//    val program = parseInput(in)
//    val diff = LazyList
//      .iterate(12342734883266L)(_ - 1)
//      .filterNot(_.toString.contains('0'))
//      .takeWhile(_ >= 11111111111111L)
//      .find { inp =>
//        val args = inp.toString.map(_.asDigit).toList
//        val optZ = Runtime.runSuperOptimized(args)
//        if (inp % 1024 == 0) println(s"$inp: $optZ")
//        optZ == 0
//      }
//
//    println(diff)

//    val optimized = optimize(program)
//    println(prettyProgram(optimized))

//    val samples = samplesInputsBetween(10)(1111111, 9999999)
//    samples.foreach { in =>
//      if (!verifyHypothesis(program, in)) {
//        println(s"Found counter-example to hypothesis: $in")
//      }
//    }
//    println(samples.map(in => in.mkString -> Runtime.run(program, in).pretty).sortBy(_._1).map(t => s"${t._1}: ${t._2}").mkString("\n"))
    Runtime.findMaxValidModelNumber.get
  }

  /*
  0: w = d0, x = 1, y = d0 + 7, z = zInit*26 + d0 + 7
  1: w = d1, x = 1, y = d1 + 8, z = zPrev*26 + d1 + 8
  2: w = d2, x = 1, y = d2 + 2, z = zPrev*26 + d2 + 2
  3: w = d3, x = 1, y = d3 + 11, z = zPrev*26 + d3 + 11
  4: w = d4, x = 1, y = d4 + 6, z = z2*26 + d4 + 6 if !(d4 == 9 && d3 == 1), otherwise w = d4, x = 0, y = 0, z = z2
  5: w = d5, x = 1, y = d5 + 12, z = z4*26 + d5 + 12
  6:
   */

  def verifyHypothesis(program: List[Instruction], input: List[Int]): Boolean = {
    val real = Runtime.run(program, input)
    val yAdditions = List(7, 8, 2, 11, 6, 12, 14)
    assert(yAdditions.size == input.size)
    def hypoZ(revDigits: List[Int], ycs: List[Int]): Long = revDigits match {
      case Nil => 0
      case head :: tail => hypoZ(tail, ycs.tail)*26 + head + ycs.head
    }
    val hz = hypoZ(input.reverse, yAdditions.reverse)
    if (real.w != input.last) println(s"HYPOTHESIS FAILED: $input, w != input.last")
    if (real.x != 1) println(s"HYPOTHESIS FAILED: $input, x != 1")
    if (real.y != input.last + yAdditions.last) println(s"HYPOTHESIS FAILED: $input, y != input.last + yAdditions.last")
    if (real.z != hz) println(s"HYPOTHESIS FAILED: $input, z != hz")
    real.w == input.last && real.x == 1 && real.y == input.last + yAdditions.last && real.z == hz
  }

  def part2(in: String) = Task.effect {
    Runtime.findMinValidModelNumber.get
  }

  val cases = List(
//        Test("example", InputString("""""".stripMargin)),
    Puzzle(ResourceInput("day24puzzle.txt"))
  )
}

/*
0:
z0 <- zInit*26 + d0 + 7

1:
z1 <- z0*26 + d1 + 8

2:
z2 <- z1*26 + d2 + 2

3:
z3 <- z2*26 + d3 + 11

4:
if ((z3 % 26) - 3 == d4) {
    z4 <- z3 / 26
} else {
    z4 <- (z3 / 26) * 26 + d4 + 6
}

5:
z5 <- z4*26 + d5 + 12

6:
z6 <- z5*26 + d6 + 14

7:
if ((z6 % 26) - 16 == d7) {
    z7 <- z6 / 26
} else {
    z7 <- (z6 / 26) * 26 + d7 + 13
}

8:
z8 <- z7*26 + d8 + 15

9:
if ((z8 % 26) - 8 == d9) {
    z9 <- z8 / 26
} else {
    z9 <- (z8 / 26) * 26 + d9 + 10
}

10:
if ((z9 % 26) - 12 == d10) {
    z10 <- z9 / 26
} else {
    z10 <- (z9 / 26) * 26 + d10 + 6
}


11:
if ((z10 % 26) - 7 == d11) {
    z11 <- z10 / 26
} else {
    z11 <- (z10 / 26) * 26 + d11 + 10
}

12:
if ((z11 % 26) - 6 == d12) {
    z12 <- z11 / 26
} else {
    z12 <- (z11 / 26) * 26 + d12 + 8
}
From 13, z12 < 26, and (z12 % 26) - 11 = d13 <==> z12 = d13 + 11
This can be viewed as a new problem, finding a thirteen digit number ending with z = d13 + 11
This should be the basis of our recursion

13:
if ((z12 % 26) - 11 == d13) { // This must be true, and z12 must be < 26
    z13 <- z12 / 26
} else {
    z13 <- (z12 / 26)*26 + d13 + 5
}
 */