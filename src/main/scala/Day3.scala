object Day3 {
  def problem1(input: List[String]): Int = {
    val gammaString = input.transpose
      .map(bits => bits.groupBy(identity).maxBy(_._2.size)._1)
      .mkString
    val gamma = Integer.parseInt(gammaString, 2)
    val epsilonString = gammaString.map(bit => if (bit == '0') '1' else '0')
    val epsilon = Integer.parseInt(epsilonString, 2)
    gamma * epsilon
  }

  def problem2(input: List[String]): Int = {
    val mostCommonInColumn = input.transpose
      .map(bits => bits.groupBy(identity).map(a => (a._1, a._2.size)))
      .map(column => if (column('1') >= column('0')) '1' else '0')

    val leastCommonInColumn =
      mostCommonInColumn.map(bit => if (bit == '0') '1' else '0')

    val o2rating = rating(input, mostCommonInColumn)
    val co2rating = rating(input, leastCommonInColumn)

    Integer.parseInt(o2rating, 2) * Integer.parseInt(co2rating, 2)
  }

  def rating(input: List[String], bitCriteria: List[Char]): String = {
    input
      .map(a => a zip bitCriteria)
      .map(a =>
        (a, a.takeWhile { case (char, commonBit) => char == commonBit })
      )
      .map { case (a, b) => (a.map(_._1).mkString, b.map(_._1).mkString) }
      .maxBy(_._2.length)
      ._1
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input3")
    println(problem1(input))
    println(problem2(input))
  }

}
