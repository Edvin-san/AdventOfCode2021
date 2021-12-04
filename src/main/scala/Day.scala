import java.io.{File, IOException}

import zio.{RIO, ZEnv, ZIO, ZManaged, console}
import Util.normalizeNewLine
import zio.blocking.Blocking
import Day.TestIOOps

trait Day[P1, P2] extends zio.App {
  def logic: RIO[ZEnv, Unit] = {
    //    console.putStrLn(s"Going to run tests for $inputs") *>
    ZIO.foreach_(inputs.toList.sortBy(_._1)) { case (label, input) =>
      for {
        _ <- console.putStrLn(s"----- $label -----")
        _ <- getInput(input).map(normalizeNewLine).use { in =>
          for {
            _ <- ZIO.foreach_(List(part1(in), part2(in)).zipWithIndex) { case (p, idx) => p.catchNotImplemented.timed.flatMap {
                case (dur, a) => console.putStrLn(s"Part${idx + 1}\t$a\t${dur.toMillis}ms")
              }
            }
          } yield ()
        }
      } yield ()
    }
  }

  def part1(in: String): RIO[ZEnv, P1]

  def part2(in: String): RIO[ZEnv, P2]

  sealed trait Input

  case class InputString(value: String) extends Input

  case class ResourceInput(value: String) extends Input

  def getInput(in: Input): ZManaged[Blocking, IOException, String] = in match {
    case InputString(value) => ZManaged.succeed(value)
    case ResourceInput(path) => {
      val resourcePath = new File(getClass.getClassLoader.getResource(path).getPath).getPath
      ZManaged.readFile(resourcePath).mapM(_.readAll(4096).mapBoth({
        case Some(iOException) => iOException
        case None => new IOException("readAll failed with None")
      }, _.map(_.toChar).mkString))
    }
  }

  def inputs: Map[String, Input]

  def run(args: List[String]) =
    logic.exitCode
}

object Day {

  implicit class TestIOOps[A](zio: RIO[ZEnv, A]) {
    def catchNotImplemented = zio.map(_.toString).catchSome {
      case _: NotImplementedError => ZIO.succeed("Not implemented.")
    }
  }

}
