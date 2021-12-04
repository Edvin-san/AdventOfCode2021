import scala.util.{Success, Try}

// Stolen from https://rosettacode.org/wiki/Chinese_remainder_theorem#Scala
object ChineseRemainderTheorem extends App {

  def chineseRemainder(n: List[BigInt], a: List[BigInt]): Option[BigInt] = {
    require(n.size == a.size)
    val prod = n.product

    def iter(n: List[BigInt], a: List[BigInt], sm: BigInt): BigInt = {
      def mulInv(a: BigInt, b: BigInt): BigInt = {
        def loop(a: BigInt, b: BigInt, x0: BigInt, x1: BigInt): BigInt = {
          if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
        }

        if (b == 1) 1
        else {
          val x1 = loop(a, b, 0, 1)
          if (x1 < 0) x1 + b else x1
        }
      }

      if (n.nonEmpty) {
        val p = prod / n.head

        iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)
      } else sm
    }

    Try {
      iter(n, a, 0) % prod
    } match {
      case Success(v) => Some(v)
      case _          => None
    }
  }

  println(chineseRemainder(List(3, 5, 7), List(2, 3, 2)))
  println(chineseRemainder(List(11, 12, 13), List(10, 4, 12)))
  println(chineseRemainder(List(11, 22, 19), List(10, 4, 9)))
  println(chineseRemainder(List(17, 13, 19), List(0, 11, 16)))

}