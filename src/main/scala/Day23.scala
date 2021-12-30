import scala.annotation.tailrec

object Day23 {
  type State = Map[Coordinate, Amphipod]
  type Graph = Map[Coordinate, Int]
  case class Coordinate(x: Int, y: Int)

  case class Amphipod(energy: Int)
  object A extends Amphipod(1)
  object B extends Amphipod(10)
  object C extends Amphipod(100)
  object D extends Amphipod(1000)

  def problem2(input: List[String]): Unit = {
    val state: State = (for {
      (row, y) <- input.zipWithIndex
      (cell, x) <- row.zipWithIndex
      amphipod <- parseAmphipod(cell)
    } yield Coordinate(x, y) -> amphipod).toMap

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
      .flatMap(a => a._1.map(b => (b, a._2)))
      .toMap

    val graph = state.map { case (coordinate, amphipod) => coordinate -> amphipod.energy }

    def dijsktra(graph: Graph, source: Coordinate): (Graph, Map[Coordinate, Coordinate]) = {
      @tailrec
      def go(active: Set[Coordinate], res: Graph, pred: Map[Coordinate, Coordinate]): (Graph, Map[Coordinate, Coordinate]) = {
        if (active.isEmpty) (res, pred)
        else {
          val node = active.minBy(res)
          val amphipod = res(node)
          val neighbors = possibleCoordinates(hallways, state, correct, allPossible, node)
          val alt = {
            for {
              n <- neighbors if cost(node, n, amphipod) < res.getOrElse(n, Int.MaxValue)
            } yield n -> cost(node, n, amphipod)
          }.toMap
          val active1 = active - node ++ alt.keys
          val preds = alt.keys.map(key => key -> node).toMap
          go(active1, res ++ alt, pred ++ preds)
        }
      }
      go(Set(source), Map(source -> 1), Map.empty)
    }

    val dij = dijsktra(graph, Coordinate(3, 2))
    println(dij)
  }

  def cost(current: Coordinate, next: Coordinate, energy: Int): Int =
    (Math.abs(current.x - next.x) + Math.abs(current.y - next.y)) * energy

  def possibleCoordinates(hallways: Set[Coordinate], state: State, correct: State, allPossible: Set[Coordinate], start: Coordinate): Set[Coordinate] = {
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
