object Day08 {
  case class Entry(signalPatterns: List[String], outputValues: List[String])

  val display: Map[Int, String] = Map(
    0 -> "abcefg",
    1 -> "cf",
    2 -> "acdeg",
    3 -> "acdfg",
    4 -> "bcdf",
    5 -> "abdfg",
    6 -> "abdefg",
    7 -> "acf",
    8 -> "abcdefg",
    9 -> "abcdfg"
  )

  def problem1(input: List[Entry]): Int = {
    input
      .map(_.outputValues)
      .flatMap(_.filter(value => Set(1, 4, 7, 8).exists(digit => display(digit).length == value.length)))
      .size
  }

  def problem2(input: List[Entry]): Int = {
    input.map { entry =>
      val patterns = entry.signalPatterns.flatMap { pattern =>
        (Some(pattern.length) collect {
          case 2 => 1
          case 3 => 7
          case 4 => 4
          case 7 => 8
        }).map(i => (i, pattern))
      }.toMap

      val rest = entry.signalPatterns.flatMap { pattern =>
        (Some(pattern.length) collect {
          case 5 =>
            if ((patterns(7) diff pattern).isEmpty) 3
            else if ((pattern intersect patterns(4)).length == 3) 5
            else 2
          case 6 =>
            if ((patterns(4) diff pattern).isEmpty) 9
            else if ((patterns(7) diff pattern).isEmpty) 0
            else 6
        }).map(i => (i, pattern))
      }.toMap

      val invertedPatterns = (patterns ++ rest).map(_.swap)
      entry.outputValues.map(value => invertedPatterns(value)).mkString.toInt
    }.sum
  }

  def main(args: Array[String]): Unit = {
    val input = Utils
      .read("input08")
      .map { line =>
        val signalPatterns +: _ :+ outputValue = line.split('|').map(_.trim).toList
        Entry(signalPatterns.split(' ').map(_.sorted).toList, outputValue.split(' ').map(_.sorted).toList)
      }
    println(problem1(input))
    println(problem2(input))
  }

}
