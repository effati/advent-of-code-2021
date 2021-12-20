object Day12 {
  type Graph = Map[String, List[String]]
  type HashGraph = Map[Int, List[Int]]

  val start = "start"
  val destination = "end"
  val hashStart = start.hashCode
  val hashDest = destination.hashCode

  def problem1(graph: Graph): Int = {
    def count(cave: String, visited: List[String]): Int = {
      if (cave == destination) 1
      else if (visited.contains(cave) && cave.head.isLower) 0
      else graph(cave).map(neighbor => count(neighbor, cave :: visited)).sum
    }
    count(start, List())
  }

  def problem2(graph: HashGraph, lowerCaves: Set[Int]): Int = {
    def count(
      cave: Int,
      visited: List[Int],
      visitedSmallCaveTwice: Int
    ): Int = {
      val isSmallAndVisited = lowerCaves.contains(cave) && visited.contains(cave)
      if (cave == hashDest) 1
      else if (isSmallAndVisited && (cave == hashStart || visitedSmallCaveTwice != -1)) 0
      else
        graph(cave)
          .map(count(_, cave :: visited, if (isSmallAndVisited) cave else visitedSmallCaveTwice))
          .sum
    }
    count(hashStart, List(), -1)
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input12").map { s =>
      val edge = s.split('-')
      edge.head -> edge.last
    }
    val graph = (input ++ input.map(_.swap)).groupMap(_._1)(_._2)

    println(problem1(graph))

    val input2 = Utils.read("input12").map { s =>
      val edge = s.split('-')
      edge.head.hashCode -> edge.last.hashCode
    }
    val graph2 = (input2 ++ input2.map(_.swap)).groupMap(_._1)(_._2)
    val lowerCaves = input.map(_._1).filter(_.head.isLower).map(_.hashCode).toSet

    println(problem2(graph2, lowerCaves))
  }

}
