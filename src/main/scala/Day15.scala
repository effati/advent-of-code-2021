object Day15 {
  type Vertex = (Int, Int)
  type Distance = Int
  type Graph = Map[Vertex, Distance]

  val Neighboring = List((-1, 0), (1, 0), (0, -1), (0, 1))

  def dijsktra(graph: Graph, source: Vertex): (Graph, Graph) = {
    var dist: Graph = graph.keys.map(_ -> Int.MaxValue).toMap.updated(source, 0)
    var prev = graph.keys.map(_ -> Int.MaxValue).toMap
    var q = graph

    while (q.nonEmpty) {
      val u = dist.filter(a => q.contains(a._1)).minBy(_._2)
      q = q.removed(u._1)

      val n2 = Neighboring.map { case (dx, dy) => (dx + u._1._1, dy + u._1._2) }
      q.filter(a => n2.contains(a._1))
        .foreach { neighbor =>
          val alt = u._2 + Math.abs(u._2 - neighbor._2)
          if (alt < neighbor._2) {
            dist = dist.updated(neighbor._1, alt)
            prev = prev.updated(u._1, u._2)
          }
        }
    }
    (dist, prev)
  }

  def main(args: Array[String]): Unit = {
    val input: Graph = Utils.read("sample15").map(_.map(_.toString.toInt).toList)
      .zipWithIndex
      .flatMap { case (row, x) =>
        row.zipWithIndex
        .map { case (col, y) => (x, y) -> col}
      }
      .toMap

    val (dist, _) = dijsktra(input, (0, 0))
    val vals = dist.keys
    val maxX = vals.map(_._1).max
    val maxY = vals.map(_._2).max
    println(dist((maxX, maxY)))
  }
}
