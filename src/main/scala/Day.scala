import Day.dayString
import Util.normalizeNewLine
import zio.{RIO, ZEnv, ZIO, console}

trait Day[P1, P2] extends zio.App {
  def logic: RIO[ZEnv, Unit] = {
    ZIO.foreach_(cases) { dayCase =>
      for {
        _ <- console.putStrLn(s"${dayCase.style}----- ${dayCase.name} -----${Console.RESET}")
        _ <- dayCase.input.getInput.map(normalizeNewLine).use { in =>
          for {
            s1 <- dayString("Part1", part1(in), dayCase.expectedOutputPart1)
            s2 <- dayString("Part2", part2(in), dayCase.expectedOutputPart2)
            table = Tabulator.formatTable(List(List("Part", "Answer", "Duration", "Verdict"), s1.toList, s2.toList))
            _ <- console.putStrLn(table)
          } yield ()
        }
      } yield ()
    }
  }

  def part1(in: String): RIO[ZEnv, P1]

  def part2(in: String): RIO[ZEnv, P2]

  def cases: List[DayCase[P1, P2]]

  def run(args: List[String]) =
    logic.exitCode
}

object Day {
  case class DayCaseSummary(part: String, answer: String, duration: String, verdict: Option[String]) {
    def toList: List[String] = List(part, answer, duration, verdict.getOrElse(""))
  }
  def dayString[P](part: String, zio: RIO[ZEnv, P], maybeExpected: Option[P]): RIO[ZEnv, DayCaseSummary] = zio.timed.either.flatMap {
    case Left(_: NotImplementedError) => ZIO.succeed(DayCaseSummary(part, "Not implemented", "", None))
    case Left(t) => ZIO.fail(t)
    case Right((dur, answer)) =>
      val verdict: Option[String] = maybeExpected.map { expected =>
        if (answer == expected) "CORRECT"
        else s"WRONG Expected $expected"
      }
      ZIO.succeed(
        DayCaseSummary(
          part,
          answer.toString,
          s"${dur.toMillis.toString} ms",
          verdict
        )
      )
  }

  implicit class TestIOOps[A](zio: RIO[ZEnv, A]) {
    def catchNotImplemented = zio.map(_.toString).catchSome {
      case _: NotImplementedError => ZIO.succeed("Not implemented.")
    }
  }

}
