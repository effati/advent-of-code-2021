import scala.annotation.tailrec

object Day10 {
  val Matching = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

  val SyntaxErrorScores = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val ClosingPoints = Map(
  ')' -> 1,
  ']' -> 2,
'}' -> 3,
    '>' -> 4
)

  def problem1(input: List[String]): Int = {
    input.flatMap { row =>
      findError(row.toList, List())
    }
      .map(SyntaxErrorScores)
      .sum
  }


  @tailrec
  def findError(chars: List[Char], stack: List[Char]): Option[Char] = {
    if (chars.size <= 1) {
      val curr = chars.head
      if (Matching.contains(curr)) {
        None
      } else {
        val pop = stack.head
        if (Matching.map(_.swap)(curr) == pop) {
          None
        } else {
          Some(curr)
        }
      }
    } else {
      val curr :: rest = chars
      if (Matching.contains(curr)) {
        findError(rest, curr :: stack)
      } else {
        val pop :: tail = stack
        if (Matching.map(_.swap)(curr) == pop) {
          findError(rest, tail)
        } else {
          Some(curr)
        }
      }
    }
  }

  def findOpen(chars: List[Char], stack: List[Char]): List[Char] = {
    if (chars.size <= 1) {
      val curr = chars.head
      if (Matching.contains(curr)) {
        curr :: stack
      } else {
        stack.tail
      }
    } else {
      val curr :: rest = chars
      if (Matching.contains(curr)) {
        findOpen(rest, curr :: stack)
      } else {
        findOpen(rest, stack.tail)
      }
    }
  }

  def problem2(input: List[String]): Long = {
    val a = input
      .map(_.toList)
      .filter(row => findError(row, List()).isEmpty)
      .map(row => findOpen(row, List()))
      .map(row => row.foldLeft(0L)((a, char) => a * 5 + ClosingPoints(Matching(char))))
      .sorted
    a.drop(a.size/2).head
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input10")
    println(problem1(input))
    println(problem2(input))
  }

}
