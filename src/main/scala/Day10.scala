import scala.annotation.tailrec

object Day10 {
  val Matching = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

  val SyntaxErrorScores = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137,
    ' ' -> 0
  )

  def problem1(input: List[String]): Int = {
    input.map { row =>
      findError(row.toList, List())
    }
      .map(SyntaxErrorScores)
      .sum
  }


  @tailrec
  def findError(chars: List[Char], stack: List[Char]): Char = {
    if (chars.size <= 1) {
      val curr = chars.head
      if (Matching.contains(curr)) {
        ' '
      } else {
        val pop = stack.head
        if (Matching.map(_.swap)(curr) == pop) {
          ' '
        } else {
          curr
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
          curr
        }
      }
    }
  }

  def problem2(input: List[String]): Int = {
    input.filter { row =>
      findError(row.toList, List())
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input10")
    println(problem1(input))
    println(problem2(input))
  }

}
