object Day25 {
  case class Coordinate(x: Int, y: Int)
  type State = Seq[Seq[Char]]
  type State2 = Map[Coordinate, Char]

//  def problem1(input: State): Int = {
//    Iterator
//      .iterate(input) {state =>
//        state.zipWithIndex.map { case (row, y) =>
//          row.zipWithIndex.map { case (cucumber, x) =>
//
//          }
//        }
//      }
//  }

  def problem1(input: State2): Int = {
    val maxX = input.map(_._1.x).max
    val maxY = input.map(_._1.y).max

    Iterator
      .iterate(input) { state =>
        val east = state
          .flatMap { case (coordinate, char) =>
            if (char != '>') Seq((coordinate, char))
            else {
              val newX = if (coordinate.x + 1 > maxX) 0 else coordinate.x + 1
              val newCoord = Coordinate(newX, coordinate.y)
              if (state(newCoord) == '.') Seq((newCoord, char), (coordinate, '.')) else Seq((coordinate, char))
            }
        }

        val res = east
          .flatMap { case (coordinate, char) =>
            if (char != 'v') Seq((coordinate, char))
            else {
              val newY = if (coordinate.y + 1 > maxY) 0 else coordinate.y + 1
              val newCoord = Coordinate(coordinate.x, newY)
              if (state(newCoord) == '.') Seq((newCoord, char), (coordinate, '.')) else Seq((coordinate, char))
            }
          }
        println(res)
        res
      }
      .zipWithIndex
      .sliding(2)
      .dropWhile { seq => seq.head._1 != seq.last._1 }
      .next()
      .head
      ._2
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("sample25").zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (cucumber, x) => Coordinate(x, y) -> cucumber}
    }.toMap
    println(problem1(input))
  }
}
