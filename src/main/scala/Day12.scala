object Day12 {
  type Graph = Map[String, List[String]]
  
  def problem1(input: Graph): Int = count(input, "start", "end")

  def count(graph: Graph, start: String, destination: String): Int = {
    def count0(cave: String, visited: List[String]): Int = {
      if (cave == destination) 1
      else if (visited.contains(cave) && cave.head.isLower) 0
      else {
        graph(cave).map(neighbor => count0(neighbor, cave :: visited)).sum
      }
    }
    count0(start, List())
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input12").map{ s => 
      val edge = s.split('-')
      edge.head -> edge.last
    }
    val graph = (input ++ input.map(_.swap)).groupMap(_._1)(_._2)
    println(problem1(graph))
  }

}
