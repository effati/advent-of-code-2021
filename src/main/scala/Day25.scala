object Day25 {
  def problem1(input: Map[(Int, Int), Char]): Int = {
    val maxX = input.map(_._1._1).max
    val funcX = (x: Int, y: Int) => (if (x + 1 > maxX) 0 else x + 1, y)
    val maxY = input.map(_._1._2).max
    val funcY = (x: Int, y: Int) => (x, if (y + 1 > maxY) 0 else y + 1)

    val functions = Seq(('>', funcX), ('v', funcY))

    Iterator
      .iterate(input) { state =>
        functions.foldLeft(state) {
          case (state2, (targetChar, func)) =>
            state2 ++ state2.flatMap {
              case ((x, y), char) =>
                if (char != targetChar) None
                else {
                  val newCoord = func(x, y)
                  if (state2(newCoord) == '.') Seq((newCoord, char), ((x, y), '.')) else None
                }
            }
        }
      }
      .zipWithIndex
      .sliding(2)
      .dropWhile(seq => seq.head._1 != seq.last._1)
      .next()
      .last
      ._2
  }

  def main(args: Array[String]): Unit = {
    val input = Utils
      .read("input25")
      .zipWithIndex
      .flatMap {
        case (row, y) =>
          row.zipWithIndex.map { case (cucumber, x) => (x, y) -> cucumber }
      }
      .toMap
    println(problem1(input))
  }
}
