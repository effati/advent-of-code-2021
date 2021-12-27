object Day23 {
  case class Coordinate(x: Int, y: Int)
  case class GridItem(coordinate: Coordinate, char: Char)
  val Energy = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)

  def problem1(input: Seq[GridItem]): Unit = {
    val letters = input.filter(_.char.isLetter)
    val hallway = input.filter(_.char == '.')
    val rooms = letters
      .map(_.coordinate.x)
      .distinct
      .sorted
      .zip(Seq('A', 'B', 'C', 'D'))
      .map { case (x, char) => char -> x }
      .toMap

    val first = letters.map(_.coordinate.y).min
    val rows = letters.partition(c => c.coordinate.y == first)
    val (correct1, wrong1) = rows._1.partition(c => c.coordinate.x == rooms(c.char))
    val (correct2, wrong2) = rows._2.partition(c => c.coordinate.x == rooms(c.char))
    val correctable = wrong1.filterNot(c => correct1.map(_.char).contains(c.char))

  }

  def main(args: Array[String]): Unit = {
    val input = Utils
      .read("input23")
      .map(_.zipWithIndex)
      .zipWithIndex
      .flatMap {
        case (row, y) =>
          row.map { case (char, x) => GridItem(Coordinate(x, y), char) }
      }
      .filter(c => c.char == '.' || c.char.isLetter)
    problem1(input)
  }

}
