import scala.collection.mutable

object Day09 {
  type Row = List[Int]
  type HeightMap = List[Row]

  val Neighbors = List((-1, 0), (1, 0), (0, -1), (0, 1))

  def problem1(input: HeightMap): Int = {
    input
      .sliding(3)
      .flatMap {
        case row1 :: row2 :: row3 :: _ =>
          row2.zipWithIndex.sliding(3).flatMap { subrow =>
            val midIdx = subrow(1)._2
            val minHoriz = subrow.minBy(_._1)
            val minVerti = List(row1(midIdx), row2(midIdx), row3(midIdx)).min
            if (minHoriz._2 == midIdx && minVerti == minHoriz._1)
              Some(1 + minHoriz._1)
            else None
          }
        case _ => None
      }
      .sum
  }

  def problem2(input: HeightMap): Int = {
    val lowPoints = input.zipWithIndex
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

    lowPoints
      .map(point => basin(input, point))
      .sorted(Ordering[Int].reverse)
      .take(3)
      .product
  }

  def basin(input: HeightMap, lowPoint: (Int, Int)): Int = {
    val (y, x) = lowPoint

    val basin = mutable.ListBuffer[(Int, Int)] { (y, x) }
    val stack = mutable.Stack[(Int, Int)] { (y, x) }

    while (stack.nonEmpty) {
      val (y, x) = stack.pop()
      for ((dy, dx) <- Neighbors) {
        val y2 = y + dy
        val x2 = x + dx
        if (input(y2)(x2) <= 8 && !basin.contains((y2, x2))) {
          basin.append((y2, x2))
          stack.append((y2, x2))
        }
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
