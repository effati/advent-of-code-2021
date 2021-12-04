object Day01 {
  def problem1(input: List[Int]): Int = {
    input
      .sliding(2)
      .map(pair => pair(0) < pair(1))
      .count(a => a)
  }

  def problem2(input: List[Int]): Int = {
    val windowSums = input
      .sliding(3)
      .map(_.sum)

    problem1(windowSums.toList)
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input01").map(_.toInt)
    println(problem1(input))
    println(problem2(input))
  }
}
