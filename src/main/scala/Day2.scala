object Day2 {

  def problem1(input: List[(String, Int)]): Int = {
    val res = input.map { line =>
      line._1 match {
        case "forward" => (line._2, 0)
        case "up" => (0, -line._2)
        case "down" => (0, line._2)
      }
    }.foldLeft((0, 0)) { case (acc, (horizontal, vertical)) => (acc._1 + horizontal, acc._2 + vertical) }

    res._1 * res._2
  }

  def problem2(input: List[(String, Int)]): Int = {
    val res = input.scanLeft((0, 0, 0)) { (acc, curr) =>
      val (horizontal, vertical, aim) = acc
      val (direction, units) = curr
      direction match {
        case "forward" => (horizontal + units, vertical + (aim * units), aim)
        case "up" => (horizontal, vertical, aim - units)
        case "down" => (horizontal, vertical, aim + units)
      }
    }

    res.last._1 * res.last._2
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input2").map(line => {
      val splttedLine = line.split(" ")
      (splttedLine(0), splttedLine(1).toInt)
    })
    println(problem1(input))
    println(problem2(input))
  }
}
