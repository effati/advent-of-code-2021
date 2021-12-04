object Day02 {

  def problem1(input: List[(String, Int)]): Int = {
    val res = input.foldLeft((0, 0)) { (acc, curr) =>
      curr._1 match {
        case "forward" => (acc._1 + curr._2, acc._2)
        case "up"      => (acc._1, acc._2 - curr._2)
        case "down"    => (acc._1, acc._2 + curr._2)
      }
    }

    res._1 * res._2
  }

  def problem2(input: List[(String, Int)]): Int = {
    val res = input.foldLeft((0, 0, 0)) { (acc, curr) =>
      curr._1 match {
        case "forward" =>
          (acc._1 + curr._2, acc._2 + (acc._3 * curr._2), acc._3)
        case "up"   => (acc._1, acc._2, acc._3 - curr._2)
        case "down" => (acc._1, acc._2, acc._3 + curr._2)
      }
    }

    res._1 * res._2
  }

  def main(args: Array[String]): Unit = {
    val input = Utils
      .read("input02")
      .map(line => {
        val splttedLine = line.split(" ")
        (splttedLine(0), splttedLine(1).toInt)
      })
    println(problem1(input))
    println(problem2(input))
  }
}
