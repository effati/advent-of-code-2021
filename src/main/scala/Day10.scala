import scala.annotation.tailrec

object Day10 {
  val Matching = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
  val SyntaxErrorScores = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val ClosingPoints = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  @tailrec
  def findError(chars: List[Char], stack: List[Char] = List()): Option[Char] = {
    val (curr, rest) = (chars.head, chars.drop(1))
    if (Matching.contains(curr)) if (rest.isEmpty) None else findError(rest, curr :: stack)
    else {
      val (pop, tail) = (stack.head, stack.drop(1))
      if (Matching.map(_.swap)(curr) == pop) if (tail.isEmpty || rest.isEmpty) None else findError(rest, tail)
      else Some(curr)
    }
  }

  @tailrec
  def findOpen(chars: List[Char], stack: List[Char] = List()): List[Char] = {
    val (curr, rest) = (chars.head, chars.drop(1))
    if (Matching.contains(curr)) if (rest.isEmpty) curr :: stack else findOpen(rest, curr :: stack)
    else if (rest.isEmpty) stack.tail
    else findOpen(rest, stack.tail)
  }

  def problem1(input: List[List[Char]]): Int = {
    input
      .flatMap(findError(_))
      .map(SyntaxErrorScores)
      .sum
  }

  def problem2(input: List[List[Char]]): Long = {
    val output = input
      .filter(findError(_).isEmpty)
      .map(findOpen(_).foldLeft(0L)((acc, char) => acc * 5 + ClosingPoints(Matching(char))))
    output.sorted.drop(output.size / 2).head
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input10").map(_.toList)
    println(problem1(input))
    println(problem2(input))
  }

}
