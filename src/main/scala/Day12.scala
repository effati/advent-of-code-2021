object Day12 {
  type Graph = Map[String, List[String]]

  def problem1(input: Graph): Int = {
    val dfs = DFS(input, "start", "end")
    println(dfs)
    dfs.size
  }

  def DFS(graph: Graph, start: String, destination: String): List[List[String]] = {
    def DFS0(v: String, visited: List[String]): List[String] = {
      if (v == destination) {
        val c = v :: visited
        println(c)
        c
      } else if (v.toLowerCase == v && visited.contains(v))
        List()
      else {
        val neighbours = graph(v).filterNot(a => visited.contains(a) && a.toLowerCase == a)
//        val smallCaves2 = if (v.toLowerCase == v) v :: smallCaves else smallCaves
        neighbours.flatMap(DFS0(_, v :: visited))
      }
    }
    graph(start).map(neigh => DFS0(neigh, List())).reverse
//    DFS0(start, List(), List()).reverse
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
