import scala.annotation.tailrec

object Day23 {
  case class Coordinate(x: Int, y: Int)

  case class Amphipod(energy: Int)
  object A extends Amphipod(1)
  object B extends Amphipod(10)
  object C extends Amphipod(100)
  object D extends Amphipod(1000)

//  case class PathData(length: Int, path: Set[Coordinate])

  def problem2(input: List[String]): Unit = {
    val state: Map[Coordinate, Int] = (for {
      (row, y) <- input.zipWithIndex
      (cell, x) <- row.zipWithIndex
      amphipod <- parseAmphipod(cell)
    } yield Coordinate(x, y) -> amphipod.energy).toMap

    val hallways: Set[Coordinate] = (for {
      (row, y) <- input.zipWithIndex
      (cell, x) <- row.zipWithIndex
      if cell == '.' && !state.keys.map(_.x).toList.contains(x)
    } yield Coordinate(x, y)).toSet

    val allPossible: Set[Coordinate] = (for {
      (row, y) <- input.zipWithIndex
      (cell, x) <- row.zipWithIndex
      if cell == '.' || cell.isLetter
    } yield Coordinate(x, y)).toSet

    val correct = state
      .keys
      .groupBy(_.x)
      .toList
      .sortBy(_._1)
      .map(_._2)
      .zip(Seq(A, B, C, D))
      .flatMap(a => a._1.map(b => (b, a._2.energy)))
      .toMap

    def dijsktra(start: Map[Coordinate, Int]): Map[Map[Coordinate, Int], Int] = {
      @tailrec
      def go(stack: Set[(Map[Coordinate, Int], Int)], res: Map[Map[Coordinate, Int], Int]): Map[Map[Coordinate, Int], Int] = {
        if (stack.isEmpty) res
        else {
          val currentState = stack.minBy(a => res(a._1))
          val futureStates2 = currentState._1.map { case (c, i) =>
            val neighbors = possibleCoordinates(hallways, currentState._1, correct, allPossible, c)
            val replacement = currentState._1.removed(c)
            neighbors.map(neighbor => (replacement ++ Map(neighbor -> i)) -> cost(c, neighbor, i))
          }
//            .filter { case (_, b) => b.nonEmpty}

          val futureStates = currentState._1.flatMap { case (c, i) =>
            val neighbors = possibleCoordinates(hallways, currentState._1, correct, allPossible, c)
            val replacement = currentState._1.removed(c)
            neighbors.map(neighbor => (replacement ++ Map(neighbor -> i)) -> cost(c, neighbor, i))
          }

          val alt = futureStates
            .withFilter { case (futureState, cost) => currentState._2 + cost < res.getOrElse(futureState, Int.MaxValue) }
            .map { case (futureState, cost) => futureState -> (currentState._2 + cost) }
          val stack1 = stack - currentState ++ alt
//          val preds = alt.keys.map(key => key -> node).toMap
          go(stack1, res ++ alt)
        }
      }
      go(Set(start), Map(start -> 0))
    }

    val dij = dijsktra(state)
    println(dij)
  }

  def cost(current: Coordinate, next: Coordinate, energy: Int): Int =
    (Math.abs(current.x - next.x) + Math.abs(current.y - next.y)) * energy

  def possibleCoordinates(hallways: Set[Coordinate], state: Map[Coordinate, Int], correct: Map[Coordinate, Int], allPossible: Set[Coordinate], start: Coordinate): Set[Coordinate] = {
    val amphipod = state(start)
    val neighbors = List((-1, 0), (1, 0), (0, -1), (0, 1))
    var path = List[Coordinate]()
    var stack = List[Coordinate] { start }

    while (stack.nonEmpty) {
      val current = stack.head
      stack = stack.tail
      path = path ++ neighbors.flatMap {
        case (dx, dy) =>
          val next = Coordinate(current.x + dx, current.y + dy)
          if (!path.contains(next) && !state.contains(next) && allPossible.contains(next)) {
            stack = next :: stack
            Some(next)
          } else None
      }
    }
    path
      .filter {coord =>
        val inHallway = hallways.contains(coord)
        val inHome = correct.get(coord).map { a => a == amphipod }.isDefined
        val validHome = inHome && !state.exists(a => a._1.x == coord.x && a._2 != amphipod) &&
          !state.exists(a => a._1.x == coord.x && a._1.y >= coord.y)
        inHallway || validHome
      }
      .toSet
  }

  def parseAmphipod(c: Char): Option[Amphipod] = {
    c match {
      case 'A' => Some(A)
      case 'B' => Some(B)
      case 'C' => Some(C)
      case 'D' => Some(D)
      case _ => None
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input23")
    problem2(input)
  }

}
