object Day06 {
  def problem1(input: List[Int]): Long = simulate(input, 80)
  def problem2(input: List[Int]): Long = simulate(input, 256)

  def init(input: List[Int]): Vector[Long] = Vector.tabulate(9)(i => input.count(_ == i))

  def simulate(input: List[Int], days: Int): Long = {
    val iter = Iterator.iterate(init(input)) { fishes =>
      val babyFishes +: rest = fishes
      rest.updated(6, rest(6) + babyFishes) :+ babyFishes
    }
    (0 until days).foreach(_ => iter.next())
    iter.next().sum
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input06").head.split(',').map(_.toInt).toList
    println(problem1(input))
    println(problem2(input))
  }

}
