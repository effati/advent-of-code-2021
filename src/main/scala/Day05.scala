object Day05 {

  case class Vertex(x: Int, y: Int)

  def problem1(input: List[String]): Int = {
    val edgeDrawingRule: ((Vertex, Vertex)) => Option[List[Vertex]] = edge =>
      if (edge._1.x == edge._2.x) {
        val range = List(edge._1.y, edge._2.y).sorted
        Some((range.head to range.last).map(y => Vertex(edge._1.x, y)).toList)
      } else if (edge._1.y == edge._2.y) {
        val range = List(edge._1.x, edge._2.x).sorted
        Some((range.head to range.last).map(x => Vertex(x, edge._1.y)).toList)
      } else {
        None
      }
    lineCrossingSolver(input, edgeDrawingRule)
  }

  def problem2(input: List[String]): Int = {
    val edgeDrawingRule: ((Vertex, Vertex)) => Option[List[Vertex]] = edge =>
      if (edge._1.x == edge._2.x) {
        Some(getRange(edge._1.y, edge._2.y).map(y => Vertex(edge._1.x, y)).toList)
      } else if (edge._1.y == edge._2.y) {
        Some(getRange(edge._1.x, edge._2.x).map(x => Vertex(x, edge._1.y)).toList)
      } else if ((edge._1.x - edge._1.y).abs == (edge._2.x - edge._2.y).abs ||
                 (edge._1.x - edge._2.x).abs == (edge._1.y - edge._2.y).abs) {
        Some((getRange(edge._1.x, edge._2.x) zip getRange(edge._1.y, edge._2.y)).map {
          case (x, y) => Vertex(x, y)
        }.toList)
      } else {
        None
      }
    lineCrossingSolver(input, edgeDrawingRule)
  }

  def lineCrossingSolver(input: List[String], edgeDrawingRule: ((Vertex, Vertex)) => Option[List[Vertex]]): Int = {
    input
      .map(_.split(" -> "))
      .map(row => {
        val vertices = row.map(vertex => {
          val edge = vertex.split(',')
          Vertex(edge.head.toInt, edge.last.toInt)
        })
        (vertices.head, vertices.last)
      })
      .flatMap(edgeDrawingRule)
      .flatten
      .groupBy(identity)
      .map(_._2.size)
      .count(_ >= 2)
  }

  def getRange(start: Int, end: Int): Range = {
    if (start <= end) start to end else start to end by -1
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input05")
    println(problem1(input))
    println(problem2(input))
  }
}
