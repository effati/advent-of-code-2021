object Day09 {
  type Row = List[Int]
  type HeightMap = List[Row]

  val Neighbors = List((-1, 0), (1, 0), (0, -1), (0, 1))

  def problem1(input: HeightMap): Int = {
    lowPoints(input).map {
      case (row, col) => 1 + input(row)(col)
    }.sum
  }

  def problem2(input: HeightMap): Int = {
    lowPoints(input)
      .map(point => basin(input, point))
      .sorted(Ordering[Int].reverse)
      .take(3)
      .product
  }

  def lowPoints(input: HeightMap): List[(Int, Int)] = {
    input.zipWithIndex
      .sliding(3)
      .flatMap {
        case row1 :: row2 :: row3 :: _ =>
          row2 match {
            case (row, rowIdx) =>
              row.zipWithIndex.sliding(3).flatMap { subrow =>
                val midIdx = subrow(1)._2
                val minHoriz = subrow.minBy(_._1)
                val minVerti = List(row1._1(midIdx), row2._1(midIdx), row3._1(midIdx)).min
                if (minHoriz._2 == midIdx && minVerti == minHoriz._1)
                  Some((rowIdx, midIdx))
                else None
              }
            case _ => None
          }
      }
      .toList
  }

  def basin(input: HeightMap, lowPoint: (Int, Int)): Int = {
    var basin = List[(Int, Int)] { lowPoint }
    var stack = List[(Int, Int)] { lowPoint }

    while (stack.nonEmpty) {
      val (y, x) = stack.head
      stack = stack.tail
      basin = basin ++ Neighbors.flatMap {
        case (dy, dx) =>
          val y2 = y + dy
          val x2 = x + dx
          if (input(y2)(x2) <= 8 && !basin.contains((y2, x2))) {
            stack = (y2, x2) :: stack
            Some((y2, x2))
          } else None
      }
    }

    basin.length
  }

  def main(args: Array[String]): Unit = {
    val maxPoint = 9
    val rawInput = Utils.read("input09").map(row => maxPoint +: row.map(_.toString.toInt).toList :+ maxPoint)
    val rowLength = rawInput.head.length
    val input = List.fill(rowLength)(maxPoint) +: rawInput :+ List.fill(rowLength)(maxPoint)
    println(problem1(input))
    println(problem2(input))
  }

}
