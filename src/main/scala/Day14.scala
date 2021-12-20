object Day14 {
  def problem1(template: String, rules: Map[String, String]): Int = {
    val res = (0 until 10)
      .foldLeft(template) { (polymer, _) =>
        polymer
          .sliding(2)
          .map { a =>
            val insertion = rules(a)
            a.head + insertion
          }
          .toSeq
          .mkString :+ polymer.last
      }
      .groupBy(identity)
      .map(a => (a._1, a._2.length))

    res.maxBy(_._2)._2 - res.minBy(_._2)._2
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input14")
    val template = input.head
    val rules = input.tail.filterNot(_.isBlank).map(_.split(" -> ")).map(rule => (rule.head, rule.last)).toMap

    println(problem1(template, rules))
  }
}
