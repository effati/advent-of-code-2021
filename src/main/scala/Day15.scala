import scala.annotation.tailrec

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

  def dijsktra2(graph: Graph, source: Vertex): (Graph, Map[Vertex, Vertex]) = {
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

  def main(args: Array[String]): Unit = {
    val input: Graph = Utils
      .read("input15")
      .map(_.map(_.toString.toInt).toList)
      .zipWithIndex
      .flatMap {
        case (row, x) =>
          row.zipWithIndex
            .map { case (col, y) => (x, y) -> col }
      }
      .toMap

    val (dist, _) = dijsktra2(input, (0, 0))
    val vals = dist.keys
    val maxX = vals.map(_._1).max
    val maxY = vals.map(_._2).max
    println(dist((maxX, maxY)))

    val input2 = Utils.read("input15").map(_.map(_.toString.toInt).toList)

    val b = (0 until 5).flatMap { i =>
      input2.map(_.map { value =>
        val newVal = value + i
        if (newVal > 9) (newVal % 10) + 1 else newVal
      })
    }.toList

    val c =
      b.map(row =>
        (0 until 5).flatMap(i =>
          row.map { value =>
            val newVal = value + i
            if (newVal > 9) (newVal % 10) + 1 else newVal
          }
        )
      )

    val a: Graph = c.zipWithIndex.flatMap {
      case (row, x) =>
        row.zipWithIndex
          .map { case (col, y) => (x, y) -> col }
    }.toMap

    val (dist2, _) = dijsktra2(a, (0, 0))
    val vals2 = dist2.keys
    val maxX2 = vals2.map(_._1).max
    val maxY2 = vals2.map(_._2).max
    println(dist2((maxX2, maxY2)))

  }
}
