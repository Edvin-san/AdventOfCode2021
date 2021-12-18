import scala.util.Try

// Inspired by "Declarative vs Executable Encodings" https://www.youtube.com/watch?v=OD1Yr48-0Js&ab_channel=Ziverge
// Using executable encoding
trait Parser[+A] { self =>
  def parse(s: String): Option[(A, String)]

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
}

object Parser {
  def succeed[A](a: A): Parser[A] = (s: String) => Some(a -> s)

  def multiple[A](min: Int, max: Option[Int])(p: Parser[A]): Parser[List[A]] = (input: String) => {
    val successfulParseResults = LazyList
      .iterate(p.parse(input))(_.flatMap { case (_, rest) => p.parse(rest) })
      .zipWithIndex
      .takeWhile { case (res, idx) => res.isDefined && max.forall(_ > idx) }
      .map(_._1.get)
      .toList
    Option.when(successfulParseResults.size >= min) {
      val lastRemainder = successfulParseResults.lastOption.map(_._2).getOrElse(input)
      successfulParseResults.map(_._1) -> lastRemainder
    }
  }
  def listOf[A]: Parser[A] => Parser[List[A]] = multiple(0, None)
  def atLeast[A](n: Int): Parser[A] => Parser[List[A]] = multiple(n, None)
  def atMost[A](n: Int): Parser[A] => Parser[List[A]] = multiple(0, Some(n))
  def exactly[A](n: Int): Parser[A] => Parser[List[A]] = multiple(n, Some(n))

  def parseChar(c: Char): Parser[Char] = (s: String) => s.headOption.filter(_ == c).map(_ -> s.tail)
  def parseString(len: Int): Parser[String] = (s: String) =>
    if (s.size >= len) Some(s.take(len) -> s.drop(len))
    else None

  val parseInt: Parser[Int] = (s: String) => Try(Integer.parseInt(s, 2)).toOption.map(i => i -> "")
  val parseBigInt: Parser[BigInt] = (s: String) => Try(BigInt(s, 2)).toOption.map(i => i -> "")

  def parseIntOfLen(len: Int): Parser[Int] = parseString(len).feedInto(parseInt)
}
