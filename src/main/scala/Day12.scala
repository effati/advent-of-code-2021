object Day12 {
  type Graph = Map[String, List[String]]

  val start = "start"
  val destination = "end"

  def problem1(graph: Graph): Int = {
    def count(cave: String, visited: List[String]): Int = {
      if (cave == destination) 1
      else if (visited.contains(cave) && cave.head.isLower) 0
      else graph(cave).map(neighbor => count(neighbor, cave :: visited)).sum
    }
    count(start, List())
  }

  def problem2(graph: Graph): Int = {
    def count(
      cave: String,
      visited: List[String],
      visitedSmallCaveTwice: String
    ): Int = {
      val isSmallAndVisited = cave.head.isLower && visited.contains(cave)
      if (cave == destination) 1
      else if (isSmallAndVisited && (cave == start || visitedSmallCaveTwice != "")) 0
      else
        graph(cave)
          .map(count(_, cave :: visited, if (isSmallAndVisited) cave else visitedSmallCaveTwice))
          .sum
    }
    count(start, List(), "")
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input12").map { s =>
      val edge = s.split('-')
      edge.head -> edge.last
    }
    val graph = (input ++ input.map(_.swap)).groupMap(_._1)(_._2)

    println(problem1(graph))
    println(problem2(graph))
  }

}
