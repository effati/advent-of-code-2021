object Day07 {

  def problem1(input: Array[Int]): Int = {
    val median = Math.round(input.drop(input.length / 2).head)
    input.map(i => (i - median).abs).sum
  }

  def problem2(input: Array[Int]): Int = {
    val mean = Math.round(input.sum / input.length)
    input.map { i =>
      val n = (i - mean).abs
      (n * (n + 1)) / 2
    }.sum
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input07").head.split(',').map(_.toInt).sorted
    println(problem1(input))
    println(problem2(input))
  }

}
