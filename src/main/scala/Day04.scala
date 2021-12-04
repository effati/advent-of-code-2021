import scala.collection.mutable.ListBuffer

object Day04 {
  def cleanInput(
      input: List[String]
  ): (List[String], List[List[List[String]]]) = {
    val buffer = ListBuffer(ListBuffer[String]())
    input foreach { e =>
      if (e.isEmpty) {
        if (buffer.last.nonEmpty) buffer += ListBuffer[String]()
      } else buffer.last += e
    }
    val cleanedInput = buffer.map(_.toList).toList

    val instructions = cleanedInput.head.head.split(',').toList
    val boards = cleanedInput.tail.map(board =>
      board.map(_.split(' ').filter(_.nonEmpty).toList)
    )
    (instructions, boards)
  }

  def bingo(board: List[List[String]]): Boolean =
    board.exists(row => row.count(_ == "-1") == row.size)

  def markBoard(
      board: List[List[String]],
      instruction: String
  ): List[List[String]] =
    board.map(row =>
      row.map(number => if (number == instruction) "-1" else number)
    )

  def winningBoardSums(
      instructions: List[String],
      boards: List[List[List[String]]]
  ): List[Int] = {
    val res = ListBuffer[Int]()
    instructions.foldLeft(boards)((boardsLeft, instruction) => {
      boardsLeft.flatMap { board =>
        val markedBoard = markBoard(board, instruction)
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
