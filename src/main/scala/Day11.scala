object Day11 {
  case class GridItem(energy: Int, coordinates: Coordinates)
  case class Coordinates(y: Int, x: Int)
  case class State(
    grid: Vector[Vector[GridItem]],
    flashCount: Int,
    flashed: Set[Coordinates],
    toFlash: Set[Coordinates]
  )

  val Neighbors: List[Coordinates] = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)).map {
    case (y, x) => Coordinates(y, x)
  }

  def toCoordinates(input: Vector[Vector[Int]]): Vector[Vector[GridItem]] = {
    input
      .map(_.zipWithIndex)
      .zipWithIndex
      .map { case (row, y) => row.map { case (energy, x) => GridItem(energy, Coordinates(y, x)) } }
  }

  def stateIterator(input: Vector[Vector[GridItem]]): Iterator[State] = {
    Iterator.iterate(State(input, 0, Set(), Set())) { state =>
      flashIfEligible(State(increment(state.grid), state.flashCount, Set(), Set()))
    }
  }

  def flashIfEligible(preFlash: State): State = {
    Iterator
      .iterate(preFlash) { state =>
        state.grid.foldLeft(state) { (currState, row) =>
          row.foldLeft(currState) { (currState2, gridItem) =>
            if (gridItem.energy <= 9) currState2
            else flash(currState2, gridItem.coordinates)
          }
        }
      }
      .sliding(2)
      .dropWhile { seq => seq.head.flashCount != seq.last.flashCount }
      .next()
      .head
  }

  def increment(grid: Vector[Vector[GridItem]]): Vector[Vector[GridItem]] =
    grid.map(row => row.map(gridItem => gridItem.copy(energy = gridItem.energy + 1)))

  def incrementNeighbors(
    grid: Vector[Vector[GridItem]],
    coordinates: Coordinates,
    toFlash: Set[Coordinates]
  ): Vector[Vector[GridItem]] = {
    val validNeighbors = Neighbors.flatMap { neighbor =>
      val y2 = coordinates.y + neighbor.y
      val x2 = coordinates.x + neighbor.x
      if ((0 to 9 contains y2) && (0 to 9 contains x2) && !toFlash.contains(Coordinates(y2, x2))) {
        Some(Coordinates(y2, x2))
      } else None
    }

    grid.map(row =>
      row.map(gridItem =>
        if (validNeighbors.contains(gridItem.coordinates)) gridItem.copy(energy = gridItem.energy + 1) else gridItem
      )
    )
  }

  def flash(state: State, coordinates: Coordinates): State = {
    if (state.flashed.contains(coordinates)) {
      state
    } else {
      val newGrid = incrementNeighbors(state.grid, coordinates, state.toFlash)
      val toBeFlashed = newGrid
        .flatMap(row => row.flatMap(gridItem => if (gridItem.energy > 9) Some(gridItem.coordinates) else None))
        .toSet
      State(
        newGrid.updated(coordinates.y, newGrid(coordinates.y).updated(coordinates.x, GridItem(0, coordinates))),
        state.flashCount + 1,
        state.flashed + coordinates,
        state.toFlash ++ toBeFlashed
      )
    }
  }

  def problem1(input: Vector[Vector[GridItem]]): Int = {
    stateIterator(input).drop(100).next().flashCount
  }

  def problem2(input: Vector[Vector[GridItem]]): Int = {
    stateIterator(input).zipWithIndex.find { case (state, _) => !state.grid.flatten.exists(_.energy != 0) }.head._2
  }

  def main(args: Array[String]): Unit = {
    val input = toCoordinates(Utils.read("input11").map(_.map(_.toString.toInt).toVector).toVector)
    println(problem1(input))
    println(problem2(input))
  }

}
