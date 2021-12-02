object Day1 {
  def main(args: Array[String]): Unit = {
    val input = Utils.read("input1").map(_.toInt)
    println(problem1(input))
    println(problem2(input))
  }

  def problem1(input: List[Int]): Int = {
    input
      .sliding(2)
      .map(pair => if (pair(0) < pair(1)) 1 else 0)
      .sum
  }

  def problem2(input: List[Int]): Int = {
    val windowSums = input
      .sliding(3)
      .map(_.sum)

    problem1(windowSums.toList)
  }
}
