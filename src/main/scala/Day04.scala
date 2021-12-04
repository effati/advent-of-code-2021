import scala.collection.mutable.ListBuffer

object Day04 {
  def cleanInput(input: List[String]): (List[String], List[List[List[String]]]) = {
    val buffer = ListBuffer(ListBuffer[String]())
    input foreach { e =>
      if (e.isEmpty) {
        if (buffer.last.nonEmpty) buffer += ListBuffer[String]()
      } else buffer.last += e
    }
    val cleanedInput = buffer.map(_.toList).toList

    val instructions = cleanedInput.head.head.split(',').toList
    val boards = cleanedInput.tail.map(board => board.map(_.split(' ').filter(_.nonEmpty).toList))
    (instructions, boards)
  }

  def winningBoardSums(instructions: List[String], boards: List[List[List[String]]]): List[Int] = {
    val bingo: List[List[String]] => Boolean = board => board.exists(row => row.count(_ == "-1") == row.size)
    val res = ListBuffer[Int]()
    instructions.foldLeft(boards)((boardsLeft, instruction) => {
      boardsLeft.flatMap { board =>
        val markedBoard = board.map(row => row.map(number => if (number == instruction) "-1" else number))
        if (bingo(markedBoard) || bingo(markedBoard.transpose)) {
          val restOfBoard = markedBoard.flatten
            .filter(_ != "-1")
            .map(_.toInt)
            .sum
          res += instruction.toInt * restOfBoard
          None
        } else { Some(markedBoard) }
      }
    })
    res.toList
  }

  def problem1and2combined(input: List[String]): (Int, Int) = {
    val (instructions, boards) = cleanInput(input)
    val allSums = winningBoardSums(instructions, boards)
    (allSums.head, allSums.last)
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input04")
    println(problem1and2combined(input))
  }
}
