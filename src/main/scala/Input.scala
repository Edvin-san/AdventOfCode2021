import Input.{InputString, ResourceInput}
import zio.ZManaged
import zio.blocking.Blocking

import java.io.{File, IOException}

sealed trait Input {
  def getInput: ZManaged[Blocking, IOException, String] = this match {
    case InputString(value) => ZManaged.succeed(value)
    case ResourceInput(path) =>
      val resourcePath = new File(getClass.getClassLoader.getResource(path).getPath).getPath
      ZManaged.readFile(resourcePath).mapM(_.readAll(4096).mapBoth({
        case Some(iOException) => iOException
        case None => new IOException("readAll failed with None")
      }, _.map(_.toChar).mkString))
  }
}

object Input {
  case class InputString(value: String) extends Input

  case class ResourceInput(value: String) extends Input
}



