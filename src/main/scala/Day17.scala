import scala.annotation.tailrec

object Day17 {
  case class Coordinate(x: Int, y: Int)
  case class Trajectory(x: Int, y: Int)
  val Start = Coordinate(0, 0)

  def tryThis(trajectory: Trajectory, xRange: Range, yRange: Range): Option[List[Coordinate]] = {
    @tailrec
    def go(coordinate: Coordinate, trajectory: Trajectory, positions: List[Coordinate]): Option[List[Coordinate]] = {
      if (coordinate.x > xRange.last || coordinate.y < yRange.head) None
      else if (xRange.contains(coordinate.x) && yRange.contains(coordinate.y)) Some(positions)
      else {
        val newCoord = Coordinate(coordinate.x + trajectory.x, coordinate.y + trajectory.y)
        val newTraj = Trajectory(
          if (trajectory.x > 0) trajectory.x - 1 else if (trajectory.x < 0) trajectory.x + 1 else 0,
          trajectory.y - 1
        )
        go(newCoord, newTraj, positions :+ coordinate)
      }
    }
    go(Start, trajectory, List())
  }

  def tryThis2(
    trajectory: Trajectory,
    xStart: Int,
    xEnd: Int,
    yStart: Int,
    yEnd: Int
  ): Option[Trajectory] = {
    @tailrec
    def go(coordinate: Coordinate, trajectory: Trajectory): Option[Trajectory] = {
      if (coordinate.x > xEnd || coordinate.y < yStart) None
      else if (xStart <= coordinate.x && coordinate.y <= yEnd) Some(trajectory)
      else {
        val newCoord = Coordinate(coordinate.x + trajectory.x, coordinate.y + trajectory.y)
        val newTraj = Trajectory(
          if (trajectory.x > 0) trajectory.x - 1 else if (trajectory.x < 0) trajectory.x + 1 else 0,
          trajectory.y - 1
        )
        go(newCoord, newTraj)
      }
    }
    go(Start, trajectory)
  }

  def problem1(input: String): Int = {
    val p = ".*x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)".r
    val p(xStart, xEnd, yStart, yEnd) = input
    val (xRange, yRange) = (xStart.toInt to xEnd.toInt, yStart.toInt to yEnd.toInt)

    LazyList
      .from(0)
      .map { i =>
        (0 to i)
          .flatMap(x =>
            (0 to i)
              .map(y => Trajectory(x, y))
          )
          .flatMap(tryThis(_, xRange, yRange))
      }
      .dropWhile(_.isEmpty)
      .take(200)
      .flatten
      .flatten
      .distinct
      .maxBy(_.y)
      .y
  }

  def problem2(input: String): Int = {
    val p = ".*x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)".r
    val p(x1, x2, y1, y2) = input
    val (xStart, xEnd, yStart, yEnd) = (x1.toInt, x2.toInt, y1.toInt, y2.toInt)

    val combinations = List((1, 1), (1, -1), (-1, 1), (-1, -1))
    val i = 1000
    (0 to i)
      .flatMap(x =>
        (0 to i)
          .flatMap(y => combinations.map { case (dx, dy) => Trajectory(x * dx, y * dy) })
      )
      .flatMap { traj =>
        val a = tryThis2(traj, xStart, xEnd, yStart, yEnd)
        if (a.nonEmpty) Some(traj) else None
      }
      .distinct
      .length
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input17").head
    println(problem1(input))
    println(problem2(input))
  }
}
