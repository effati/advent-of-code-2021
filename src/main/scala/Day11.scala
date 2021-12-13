object Day11 {
  case class GridItem(energy: Int, coordinates: Coordinates)

  case class Coordinates(y: Int, x: Int)

  case class State(
    grid: Vector[Vector[GridItem]],
    flashCount: Long,
    flashed: Set[Coordinates]
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

  def flashes(input: Vector[Vector[GridItem]], steps: Int): Long = {
    val iter = Iterator.iterate(State(input, 0L, Set())) { state =>
      val stepState = input.foldLeft(state)((currState, row) => {
        row.foldLeft(currState)((currState2, gridItem) => {
          flash(currState2, gridItem.coordinates, checkNeighbors = true)
        })
      })
      State(stepState.grid, stepState.flashCount, Set())
    }
    (0 until steps).foreach(_ => iter.next())

    iter.next().flashCount
  }

  def increment(grid: Vector[Vector[GridItem]]): Vector[Vector[GridItem]] =
    grid.map(row => row.map(gridItem => GridItem(gridItem.energy + 1, gridItem.coordinates)))

  def flash(
    state: State,
    coordinates: Coordinates,
    checkNeighbors: Boolean
  ): State = {
    val y = coordinates.y
    val x = coordinates.x
    if (state.flashed.contains(coordinates)) {
      state
    } else {
      val newEnergy = state.grid(y)(x).energy + 1
      if (newEnergy < 9) {
        State(
          state.grid.updated(y, state.grid(y).updated(x, GridItem(newEnergy, coordinates))),
          state.flashCount,
          state.flashed
        )
      } else {
        val newState = State(
          state.grid.updated(y, state.grid(y).updated(x, GridItem(0, coordinates))),
          state.flashCount + 1,
          state.flashed + coordinates
        )
        if (checkNeighbors) {
          val validNeighbors = Neighbors.flatMap { neighbor =>
            val y2 = y + neighbor.y
            val x2 = x + neighbor.x
            if ((0 to 9 contains y2) && (0 to 9 contains x2)) Some(Coordinates(y2, x2))
            else None
          }
          validNeighbors.foldLeft(newState)((currState, neighbor) => {
            flash(currState, neighbor, checkNeighbors = false)
          })
        } else newState
      }
    }
  }

  def problem1(input: Vector[Vector[GridItem]]): Long = flashes(input, 100)

  def main(args: Array[String]): Unit = {
    val input = toCoordinates(Utils.read("sample11").map(_.map(_.toString.toInt).toVector).toVector)
    println(problem1(input))
  }

}
