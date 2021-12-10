object Day05 {
  case class Vertex(x: Int, y: Int)

  def getRange(start: Int, end: Int): Range = if (start <= end) start to end else start to end by -1
  def isHorizontal(edge: (Vertex, Vertex)): Boolean = edge._1.x == edge._2.x
  def isVertical(edge: (Vertex, Vertex)): Boolean = edge._1.y == edge._2.y
  def isDiagonal(edge: (Vertex, Vertex)): Boolean =
    (edge._1.x - edge._1.y).abs == (edge._2.x - edge._2.y).abs ||
      (edge._1.x - edge._2.x).abs == (edge._1.y - edge._2.y).abs
  def drawVertical(edge: (Vertex, Vertex)): List[Vertex] =
    getRange(edge._1.x, edge._2.x).map(x => Vertex(x, edge._1.y)).toList
  def drawHorizontal(edge: (Vertex, Vertex)): List[Vertex] =
    getRange(edge._1.y, edge._2.y).map(y => Vertex(edge._1.x, y)).toList
  def drawDiagonal(edge: (Vertex, Vertex)): List[Vertex] =
    getRange(edge._1.x, edge._2.x)
      .zip(getRange(edge._1.y, edge._2.y))
      .map { case (x, y) => Vertex(x, y) }
      .toList

  def lineCrossingSolver(input: List[String], edgeDrawingRule: ((Vertex, Vertex)) => Option[List[Vertex]]): Int = {
    input
      .map(_.split(" -> "))
      .map { row =>
        val vertices = row.map { vertex =>
          val edge = vertex.split(',')
          Vertex(edge.head.toInt, edge.last.toInt)
        }
        (vertices.head, vertices.last)
      }
      .flatMap(edgeDrawingRule)
      .flatten
      .groupBy(identity)
      .map(_._2.size)
      .count(_ >= 2)
  }

  def problem1(input: List[String]): Int = {
    val edgeDrawingRule: ((Vertex, Vertex)) => Option[List[Vertex]] = edge =>
      if (isHorizontal(edge)) Some(drawHorizontal(edge))
      else if (isVertical(edge)) Some(drawVertical(edge))
      else None
    lineCrossingSolver(input, edgeDrawingRule)
  }

  def problem2(input: List[String]): Int = {
    val edgeDrawingRule: ((Vertex, Vertex)) => Option[List[Vertex]] = edge =>
      if (isHorizontal(edge)) Some(drawHorizontal(edge))
      else if (isVertical(edge)) Some(drawVertical(edge))
      else if (isDiagonal(edge)) Some(drawDiagonal(edge))
      else None
    lineCrossingSolver(input, edgeDrawingRule)
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input05")
    println(problem1(input))
    println(problem2(input))
  }
}
