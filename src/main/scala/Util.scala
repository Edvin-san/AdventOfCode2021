object Util {
  implicit class MapOps[K, V](m: Map[K, V]) {
    def mapValuesWith[A](f: V => A): Map[K, A] = m.view.mapValues(f).toMap
  }

  implicit class ListOps[A](xs: List[A]) {
    def frequency: Map[A, Int] = xs.groupBy(identity).mapValuesWith(_.size)
  }

  implicit class LazyListOps[A](lzy: LazyList[A]) {
    def getFirstRepeated: A = lzy.grouped(2).find(it => it.head == it.last).get.head
  }

  def lowerBound[V <% Ordered[V]](first: BigInt, last: BigInt, value: V, valueOf: BigInt => V): BigInt = {
    // Adjusted from https://en.cppreference.com/w/cpp/algorithm/lower_bound
    var f = first
    var count: BigInt = last - first

    while (count > 0) {
      val step = count / 2
      val i = f + step
      if (valueOf(i) < value) {
        f = i + 1
        count -= step + 1
      } else count = step
    }

    f
  }

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }

  def posMod(a: BigInt, m: BigInt): BigInt = (a % m + m) % m

  def modularInverse(a: BigInt, m: BigInt): BigInt = {
    val (_, x, _) = gcdExtended(a, m)
    (x % m + m) % m
  }

  // Return gcd(a, b), x, y such that a*x + b*y = gcd(a, b)
  def gcdExtended(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt) = {
    if (a == 0) (b, 0, 1)
    else {
      val (gcd, x1, y1) = gcdExtended(b % a, a)
      val x = y1 - (b / a) * x1
      val y = x1

      (gcd, x, y)
    }
  }

  def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)

  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a.abs else gcd(b, a % b)

  def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(BigInt(1))((a, b) => (a / gcd(a, b)) * b)

  def ceilDiv(a: Long, b: Long): Long = (a + b - 1) / b

  object Vector {

    case class Pos(x: Int, y: Int) {
      def +(direction: Dir): Pos = Pos(x + direction.x, y + direction.y)
      def -(p: Pos): Pos = Pos(x - p.x, y - p.y)
      def length: Double = math.sqrt(x*x + y*y)
      def toDir = Dir(x, y)
    }

    case class Dir(x: Int, y: Int) {
      def +(direction: Dir): Dir = Dir(x + direction.x, y + direction.y)
      def unary_- = Dir(-x, -y)

      def *(i: Int) = Dir(x * i, y * i)

      def rotated90CCW: Dir = {
        val pc = PolarCoordinate.make(Pos(x, y))
        val newPc = pc.rotated(math.Pi/2)
        newPc.toCartesianRounded.toDir
      }

      def rotated90CW: Dir = {
        val pc = PolarCoordinate.make(Pos(x, y))
        val newPc = pc.rotated(-math.Pi/2)
        newPc.toCartesianRounded.toDir
      }
    }

    case class PolarCoordinate(r: Double, theta: Double) {
      def toCartesianRounded = {
        val (x, y) = toCartesian
        Pos(x.round.toInt, y.round.toInt)
      }

      def toCartesian = (r*math.cos(theta), r*math.sin(theta))
      def rotated(radians: Double) = this.copy(theta = theta + radians)
    }

    object PolarCoordinate {
      def make(p: Pos): PolarCoordinate =
        PolarCoordinate(p.length, math.atan2(p.y, p.x))
    }

    def dist(p1: Pos, p2: Pos): Double = (p1 - p2).length

    object Dir {
      val North = Dir(0, 1)
      val NorthEast = Dir(1, 1)
      val East = Dir(1, 0)
      val SouthEast = Dir(1, -1)
      val South = Dir(0, -1)
      val SouthWest = Dir(-1, -1)
      val West = Dir(-1, 0)
      val NorthWest = Dir(-1, 1)

      val all8Dirs = List(
        Dir.North,
        Dir.NorthEast,
        Dir.East,
        Dir.SouthEast,
        Dir.South,
        Dir.SouthWest,
        Dir.West,
        Dir.NorthWest)
    }

  }

  def normalizeNewLine(s: String) = s.replace("\r\n", "\n")
}