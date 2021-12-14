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
      possibleFlash(State(increment(state.grid), state.flashCount, Set(), Set()))
    }
  }

  def possibleFlash(preFlash: State): State = {
    var postFlash = preFlash
    var prevCount = 0L
    do {
      prevCount = postFlash.flashCount
      postFlash = postFlash.grid.foldLeft(postFlash)((currState, row) => {
        row.foldLeft(currState)((currState2, gridItem) => {
          if (gridItem.energy <= 9) currState2
          else flash(currState2, gridItem.coordinates)
        })
      })
    } while (prevCount != postFlash.flashCount)
    postFlash
  }

  def increment(grid: Vector[Vector[GridItem]]): Vector[Vector[GridItem]] =
    grid.map(row => row.map(gridItem => gridItem.copy(energy = gridItem.energy + 1)))

  def incrementNeighbors(grid: Vector[Vector[GridItem]], neighbors: List[Coordinates]): Vector[Vector[GridItem]] = {
    grid.map(row =>
      row.map(gridItem =>
        if (neighbors.contains(gridItem.coordinates)) gridItem.copy(energy = gridItem.energy + 1) else gridItem
      )
    )
  }

  def flash(state: State, coordinates: Coordinates): State = {
    val y = coordinates.y
    val x = coordinates.x
    if (state.flashed.contains(coordinates)) {
      state
    } else {
      val newGrid = incrementNeighbors(state.grid, validNeighbors(y, x, state.toFlash))
      val toBeFlashed = newGrid
        .flatMap(row => row.flatMap(gridItem => if (gridItem.energy > 9) Some(gridItem.coordinates) else None))
        .toSet
      State(
        updateGrid(newGrid, coordinates, 0),
        state.flashCount + 1,
        state.flashed + coordinates,
        state.toFlash ++ toBeFlashed
      )
    }
  }

  def updateGrid(
    grid: Vector[Vector[GridItem]],
    coordinates: Coordinates,
    newValue: Int
  ): Vector[Vector[GridItem]] =
    grid.updated(coordinates.y, grid(coordinates.y).updated(coordinates.x, GridItem(newValue, coordinates)))

  def validNeighbors(
    y: Int,
    x: Int,
    toFlash: Set[Coordinates]
  ): List[Coordinates] = {
    Neighbors.flatMap { neighbor =>
      val y2 = y + neighbor.y
      val x2 = x + neighbor.x
      if ((0 to 9 contains y2) && (0 to 9 contains x2) && !toFlash.contains(Coordinates(y2, x2))) {
        Some(Coordinates(y2, x2))
      } else None
    }
  }

  def problem1(input: Vector[Vector[GridItem]]): Int = {
    val iter = stateIterator(input)
    (0 until 100).foreach(_ => iter.next())
    iter.next().flashCount
  }

  def problem2(input: Vector[Vector[GridItem]]): Int = {
    var synced = false
    var step = 0
    val iter = stateIterator(input)

    while (!synced) {
      if (!iter.next().grid.flatten.exists(_.energy != 0)) synced = true
      step += 1
    }

    step
  }

  def main(args: Array[String]): Unit = {
    val input = toCoordinates(Utils.read("input11").map(_.map(_.toString.toInt).toVector).toVector)
    println(problem1(input))
    println(problem2(input))
  }

}
