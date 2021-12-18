import scala.util.Try

// Inspired by "Declarative vs Executable Encodings" https://www.youtube.com/watch?v=OD1Yr48-0Js&ab_channel=Ziverge
// Using executable encoding
trait Parser[+A] { self =>
  def parse(s: String): Option[(A, String)]
  def unsafeParse(s: String): A = parse(s).get._1

  def flatMap[B](f: A => Parser[B]): Parser[B] = (s: String) => self.parse(s) match {
    case Some((a, remainder)) =>
      f(a).parse(remainder)
    case None => None
  }

  def map[B](f: A => B): Parser[B] = flatMap(a => Parser.succeed(f(a)))

  def withFilter(f: A => Boolean): Parser[A] = (s: String) => self.parse(s) match {
    case Some((a, remainder)) => Option.when(f(a))(a -> remainder)
    case None => None
  }

  def feedInto[B](p: Parser[B])(implicit ev: A <:< String): Parser[B] = (s: String) => self.parse(s).flatMap {
    case (a, remainder) => p.parse(a).map { case (b, _) => b -> remainder }
  }

  def *>[B](other: Parser[B]): Parser[B] = flatMap(_ => other)

  def orElse[B >: A](other: Parser[B]): Parser[B] = (s: String) => self.parse(s) match {
    case None => other.parse(s)
    case success => success
  }
}

object Parser {
  def succeed[A](a: A): Parser[A] = (s: String) => Some(a -> s)

  def multiple[A](min: Int, max: Option[Int], separator: Option[String])(p: Parser[A]): Parser[List[A]] = (input: String) => {
    val sepParser = separator match {
      case Some(sep) => eatString(sep) *> p
      case None => p
    }
    val successfulParseResults = LazyList
      .iterate(p.parse(input))(_.flatMap { case (_, rest) => sepParser.parse(rest) })
      .zipWithIndex
      .takeWhile { case (res, idx) => res.isDefined && max.forall(_ > idx) }
      .map(_._1.get)
      .toList
    Option.when(successfulParseResults.size >= min) {
      val lastRemainder = successfulParseResults.lastOption.map(_._2).getOrElse(input)
      successfulParseResults.map(_._1) -> lastRemainder
    }
  }
  def listOf[A]: Parser[A] => Parser[List[A]] = multiple(0, None, None)
  def atLeast[A](n: Int): Parser[A] => Parser[List[A]] = multiple(n, None, None)
  def atMost[A](n: Int): Parser[A] => Parser[List[A]] = multiple(0, Some(n), None)
  def exactly[A](n: Int): Parser[A] => Parser[List[A]] = multiple(n, Some(n), None)
  def separatedBy[A](separator: String): Parser[A] => Parser[List[A]] = multiple(0, None, Some(separator))

  def parseChar(c: Char): Parser[Char] = (s: String) => s.headOption.filter(_ == c).map(_ -> s.tail)
  def eatString(yummy: String): Parser[Unit] = for {
    eaten <- parseStringOfLen(yummy.length)
    if eaten == yummy
  } yield ()
  def parseStringOfLen(len: Int): Parser[String] = (s: String) =>
    if (s.size >= len) Some(s.take(len) -> s.drop(len))
    else None

  def parseInt(base: Int): Parser[Int] = (s: String) => Try(Integer.parseInt(s, base)).toOption.map(i => i -> "")
  def parseBigInt(base: Int): Parser[BigInt] = (s: String) => Try(BigInt(s, base)).toOption.map(i => i -> "")

  def parseIntOfLen(len: Int, base: Int): Parser[Int] = parseStringOfLen(len).feedInto(parseInt(base))
  def parseIntOfLen(len: Int): Parser[Int] = parseIntOfLen(len, 10)
}
