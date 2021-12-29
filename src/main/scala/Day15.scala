import scala.annotation.tailrec

object Day15 {
  type Vertex = (Int, Int)
  type Distance = Int
  type Graph = Map[Vertex, Distance]

  val Neighboring = List((-1, 0), (1, 0), (0, -1), (0, 1))

  def dijsktra(graph: Graph, source: Vertex): (Graph, Map[Vertex, Vertex]) = {
    @tailrec
    def go(active: Set[Vertex], res: Graph, pred: Map[Vertex, Vertex]): (Graph, Map[Vertex, Vertex]) = {
      if (active.isEmpty) (res, pred)
      else {
        val node = active.minBy(res)
        val cost = res(node)
        val neighbors = graph.keys.filter(Neighboring.map { case (dx, dy) => (dx + node._1, dy + node._2) }.contains)
        val alt = {
          for {
            n <- neighbors if cost + graph(n) < res.getOrElse(n, Int.MaxValue)
          } yield n -> (cost + graph(n))
        }.toMap
        val active1 = active - node ++ alt.keys
        val preds = alt.keys.map(key => key -> node).toMap
        go(active1, res ++ alt, pred ++ preds)
      }
    }
    go(Set(source), Map(source -> 0), Map.empty)
  }

  def shortestDistance(input: Seq[Seq[Distance]]): Distance = {
    val input2 = input
      .zipWithIndex
      .flatMap {
        case (row, x) =>
          row.zipWithIndex
            .map { case (col, y) => (x, y) -> col }
      }
      .toMap

    val (dist, _) = dijsktra(input2, (0, 0))
    val vals = dist.keys
    val maxX = vals.map(_._1).max
    val maxY = vals.map(_._2).max
    dist((maxX, maxY))
  }
  
  def problem1(input: List[List[Int]]): Int = shortestDistance(input)

  def problem2(input: List[List[Int]]): Int = {
    val expandedY = (0 until 5).flatMap { i =>
      input.map(_.map { value =>
        val newVal = value + i
        if (newVal > 9) (newVal % 10) + 1 else newVal
      })
    }.toList

    val expanded =
      expandedY.map(row =>
        (0 until 5).flatMap(i =>
          row.map { value =>
            val newVal = value + i
            if (newVal > 9) (newVal % 10) + 1 else newVal
          }
        )
      )

    shortestDistance(expanded)
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input15").map(_.map(_.toString.toInt).toList)

    println(problem1(input))
    println(problem2(input))
  }
}
