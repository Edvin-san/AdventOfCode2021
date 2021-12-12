

sealed trait DayCase[+P1Output, +P2Output] {
  def name: String
  def input: Input
  def expectedOutputPart1: Option[P1Output]
  def expectedOutputPart2: Option[P2Output]
  def style: String
}

object DayCase {
  case class Test[P1Output, P2Output](name: String, input: Input, answerPart1: Option[P1Output] = None, answerPart2: Option[P2Output] = None)
    extends DayCase[P1Output, P2Output] {
    override def expectedOutputPart1: Option[P1Output] = answerPart1
    override def expectedOutputPart2: Option[P2Output] = answerPart2
    override def style: String = s"${Console.BOLD}${Console.BLUE}"
  }

  object Test {
    def apply(name: String, input: Input): Test[Nothing, Nothing] = Test(name, input, None, None)
    def apply[P1](name: String, input: Input, p1answer: P1): Test[P1, Nothing] = Test(name, input, Some(p1answer), None)
    def applyP2[P2](name: String, input: Input, p2answer: P2): Test[Nothing, P2] = Test(name, input, None, Some(p2answer))
    def apply[P1, P2](name: String, input: Input, p1answer: P1, p2answer: P2): Test[P1, P2] = Test(name, input, Some(p1answer), Some(p2answer))
  }

  case class Puzzle[P1Output, P2Output](input: Input, answerPart1: Option[P1Output] = None, answerPart2: Option[P2Output] = None)
    extends DayCase[P1Output, P2Output] {
    override def name: String = "Puzzle"
    override def expectedOutputPart1: Option[P1Output] = answerPart1
    override def expectedOutputPart2: Option[P2Output] = answerPart2
    override def style: String = s"${Console.BOLD}${Console.CYAN}"
  }

  object Puzzle {
    def apply(input: Input): Puzzle[Nothing, Nothing] = Puzzle(input, None, None)
    def apply[P1](input: Input, p1answer: P1): Puzzle[P1, Nothing] = Puzzle(input, Some(p1answer), None)
    def applyP2[P2](input: Input, p2answer: P2): Puzzle[Nothing, P2] = Puzzle(input, None, Some(p2answer))
    def apply[P1, P2](input: Input, p1answer: P1, p2answer: P2): Puzzle[P1, P2] =
      Puzzle(input, Some(p1answer), Some(p2answer))
  }
}

