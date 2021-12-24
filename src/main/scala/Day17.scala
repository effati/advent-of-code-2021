import scala.annotation.tailrec

object Day17 {
  case class Coordinate(x: Int, y: Int)
  case class Velocity(x: Int, y: Int)
  case class Area(
    x1: Int,
    x2: Int,
    y1: Int,
    y2: Int
  )
  val Start = Coordinate(0, 0)

  def extractHits(velocity: Velocity, area: Area): Option[List[Coordinate]] = {
    @tailrec
    def go(coordinate: Coordinate, velocity: Velocity, probeHistory: List[Coordinate]): Option[List[Coordinate]] = {
      if (coordinate.x > area.x2 || coordinate.y < area.y1) None
      else if (area.x1 <= coordinate.x && coordinate.y <= area.y2) Some(probeHistory)
      else {
        val nextCoordinate = Coordinate(coordinate.x + velocity.x, coordinate.y + velocity.y)
        val nextVelocity = Velocity(
          if (velocity.x > 0) velocity.x - 1 else if (velocity.x < 0) velocity.x + 1 else 0,
          velocity.y - 1
        )
        go(nextCoordinate, nextVelocity, probeHistory :+ nextCoordinate)
      }
    }
    go(Start, velocity, List(Start))
  }

  def hits(input: String): Seq[(Velocity, List[Coordinate])] = {
    val p = ".*x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)".r
    val p(x1, x2, y1, y2) = input
    val area = Area(x1.toInt, x2.toInt, y1.toInt, y2.toInt)

    val combinations = List((1, 1), (1, -1), (-1, 1), (-1, -1))
    (0 to area.x2)
      .flatMap(x =>
        (0 to Math.abs(area.y1)).flatMap(y => combinations.map { case (dx, dy) => Velocity(x * dx, y * dy) })
      )
      .flatMap(velocity =>
        extractHits(velocity, area) match {
          case Some(res) => Some((velocity, res))
          case _         => None
        }
      )
  }

  def problem1(input: String): Int = {
    hits(input)
      .map(_._2)
      .distinct
      .flatten
      .maxBy(_.y)
      .y
  }

  def problem2(input: String): Int = {
    hits(input)
      .map(_._1)
      .distinct
      .length
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input17").head
    println(problem1(input))
    println(problem2(input))
  }
}
