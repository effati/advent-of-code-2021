object Day25 {
  case class Coordinate(x: Int, y: Int)

  def problem1(input: Map[Coordinate, Char]): Int = {
    val maxX = input.map(_._1.x).max
    val maxY = input.map(_._1.y).max

//    val funcX: Coordinate => Coordinate = coordinate => Coordinate(if (coordinate.x + 1 > maxX) 0 else coordinate.x + 1, coordinate.y)
//    val funcY: Coordinate => Coordinate = coordinate => Coordinate(coordinate.x, if (coordinate.y + 1 > maxY) 0 else coordinate.y + 1)

    Iterator
      .iterate(input) { state =>
//        Seq(('>', funcX), ('v', funcY)).foldLeft(state) { case (state2, (targetChar, func)) =>
//          state2.flatMap { case (coordinate, char) =>
//            if (char != targetChar) None
//            else {
//              val newCoord = func(coordinate)
//              if (state2(newCoord) == '.') Seq((newCoord, char), (coordinate, '.')) else None
//            }
//          }
//        }

        val moveEast = state
          .flatMap { case (coordinate, char) =>
            if (char != '>') None
            else {
              val newCoord = Coordinate(if (coordinate.x + 1 > maxX) 0 else coordinate.x + 1, coordinate.y)
              if (state(newCoord) == '.') Seq((newCoord, char), (coordinate, '.')) else None
            }
        }

        val state2 = state ++ moveEast

        val moveSouth = state2
          .flatMap { case (coordinate, char) =>
            if (char != 'v') None
            else {
              val newCoord = Coordinate(coordinate.x, if (coordinate.y + 1 > maxY) 0 else coordinate.y + 1)
              if (state2(newCoord) == '.') Seq((newCoord, char), (coordinate, '.')) else None
            }
          }

        state2 ++ moveSouth
      }
      .zipWithIndex
      .sliding(2)
      .dropWhile(seq => seq.head._1 != seq.last._1)
      .next()
      .head
      ._2
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input25").zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (cucumber, x) => Coordinate(x, y) -> cucumber}
    }.toMap
    println(problem1(input))
  }
}
