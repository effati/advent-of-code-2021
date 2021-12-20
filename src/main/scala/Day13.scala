object Day13 {
  def problem1(coordinates: Set[(Int, Int)], instructions: Seq[(Char, Int)]): Int =
    foldPaper(coordinates, instructions.head).size

  def problem2(coordinates: Set[(Int, Int)], instructions: Seq[(Char, Int)]): Set[(Int, Int)] =
    instructions.foldLeft(coordinates)((coords, instruction) => foldPaper(coords, instruction))

  def foldPaper(coords: Set[(Int, Int)], instruction: (Char, Int)): Set[(Int, Int)] = {
    val (pivot, foldLine) = instruction
    if (pivot == 'x')
      coords.map(coordinate =>
        if (coordinate._1 > foldLine) (foldLine - (coordinate._1 - foldLine), coordinate._2) else coordinate
      )
    else
      coords.map(coordinate =>
        if (coordinate._2 > foldLine) (coordinate._1, foldLine - (coordinate._2 - foldLine)) else coordinate
      )
  }

  def prettyPrint(coordinates: Set[(Int, Int)]): Unit = {
    val maxX = coordinates.maxBy(_._1)._1
    val maxY = coordinates.maxBy(_._2)._2
    (0 to maxY).foreach { y =>
      println()
      (0 to maxX).foreach(x => if (coordinates.contains((x, y))) print("###") else print("   "))
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input13")
    val coordinates = input
      .filter(_.contains(','))
      .map { line =>
        val tuple = line.split(',')
        (tuple.head.toInt, tuple.last.toInt)
      }
      .toSet
    val instructions = input
      .filterNot(line => line.contains(',') || line.isBlank)
      .map { line =>
        val tuple = line.replaceAll("fold along ", "").split('=')
        (tuple.head.head, tuple.last.toInt)
      }
    println(problem1(coordinates, instructions))
    prettyPrint(problem2(coordinates, instructions))
  }

}
