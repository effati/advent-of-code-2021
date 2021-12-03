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
    var o2rating = input
    for (index <- List.range(0, input.head.length) if o2rating.size > 1) {
      val mostCommonInColumn = mostCommon(o2rating, index)
      o2rating = o2rating.filter(row => row.charAt(index) == mostCommonInColumn)
    }

    var co2rating = input
    for (index <- List.range(0, input.head.length) if co2rating.size > 1) {
      val mostCommonInColumn = if (mostCommon(co2rating, index) == '1') '0' else '1'
      co2rating = co2rating.filter(row => row.charAt(index) == mostCommonInColumn)
    }

    Integer.parseInt(o2rating.head, 2) * Integer.parseInt(co2rating.head, 2)
  }

  def mostCommon(input: List[String], position: Int): Char = {
    input
      .transpose
      .apply(position)
      .groupBy(identity)
      .map(a => (a._1, a._2.size))
      .maxBy(_._2)
      ._1
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
