import scala.collection.mutable.ListBuffer

object Day4 {
  def cleanInput(input: List[String]): (List[String], List[List[List[String]]]) = {
    val buffer = ListBuffer(ListBuffer[String]())
    input foreach { e =>
      if (e.isEmpty) {
        if (buffer.last.nonEmpty) buffer += ListBuffer[String]()
      }
      else buffer.last += e
    }
    val cleanedInput = buffer.map(_.toList).toList

    val instructions = cleanedInput.head.head.split(',').toList
    val boards = cleanedInput.tail.map(board => board.map(_.split(' ').filter(_.nonEmpty).toList))
    (instructions, boards)
  }

  def problem1(input: List[String]): Int = {
    var (instructions, boards) = cleanInput(input)
    var finalInstruction = 0
    var unmarkedSum = 0
    for (instruction <- instructions if finalInstruction == 0) {
      boards = boards.map { board =>
        val markedBoard = board.map {row =>
          row.map(number => if (number == instruction) "-1" else number)
        }
        if (markedBoard.exists(row => row.count(number => number == "-1") == row.size) ||
          markedBoard.transpose.exists(col => col.count(number => number == "-1") == col.size)) {
          finalInstruction = instruction.toInt
          unmarkedSum = markedBoard.flatten.filter(number => number != "-1").map(_.toInt).sum
        }
        markedBoard
      }
    }
    finalInstruction * unmarkedSum
  }

  def problem2(input: List[String]): Int = {
    var (instructions, boards) = cleanInput(input)
    var finalInstruction = 0
    var unmarkedSum = 0
    for (instruction <- instructions) {
      boards = boards.flatMap { board =>
        val markedBoard = board.map {row =>
          row.map(number => if (number == instruction) "-1" else number)
        }
        if (markedBoard.exists(row => row.count(number => number == "-1") == row.size) ||
          markedBoard.transpose.exists(col => col.count(number => number == "-1") == col.size)) {
          finalInstruction = instruction.toInt
          unmarkedSum = markedBoard.flatten.filter(number => number != "-1").map(_.toInt).sum
          None
        } else {
          Some(markedBoard)
        }
      }
    }
    finalInstruction * unmarkedSum
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input4")
    println(problem1(input))
    println(problem2(input))
  }
}
