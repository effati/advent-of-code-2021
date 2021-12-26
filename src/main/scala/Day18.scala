import scala.annotation.tailrec

object Day18 {
  sealed trait Tree {
    def magnitude: Int
    def addLeft(value: Int): Tree
    def addRight(value: Int): Tree
    def explode(depth: Int): Exploded
    def split: Tree
  }

  case class Pair(left: Tree, right: Tree) extends Tree {
    def magnitude: Int = 3 * left.magnitude + 2 * right.magnitude
    def addLeft(value: Int): Tree = Pair(left.addLeft(value), right)
    def addRight(value: Int): Tree = Pair(left, right.addRight(value))
    def explode(depth: Int): Exploded = {
      (left, right) match {
        case (Number(l), Number(r)) if depth <= 0 => Exploded(Number(0), l, r)
        case _ =>
          val leftExp = left.explode(depth - 1)
          val nr = if (leftExp.addRight > 0) right.addLeft(leftExp.addRight) else right
          val rightExp = nr.explode(depth - 1)
          val nl = if (rightExp.addLeft > 0) leftExp.node.addRight(rightExp.addLeft) else leftExp.node
          Exploded(Pair(nl, rightExp.node), leftExp.addLeft, rightExp.addRight)
      }
    }
    def split: Tree = {
      val nl = left.split
      if (nl != left) Pair(nl, right) else Pair(left, right.split)
    }
  }

  case class Number(value: Int) extends Tree {
    def magnitude: Int = value
    def addLeft(value2: Int): Tree = Number(value + value2)
    def addRight(value2: Int): Tree = Number(value + value2)
    def explode(depth: Int): Exploded = Exploded(this, 0, 0)
    def split: Tree = {
      if (value >= 10) {
        val newLeft = value / 2
        Pair(Number(newLeft), Number(value - newLeft))
      } else Number(value)
    }
  }

  case class Exploded(node: Tree, addLeft: Int, addRight: Int)

  private def add(a: Tree, b: Tree): Tree = reduce(Pair(a, b))

  @tailrec
  private def reduce(node: Tree): Tree = {
    val next = node.explode(4).node.split
    if (next == node) next
    else reduce(next)
  }

  def parse(input: String): Tree = {
    var pos = -1
    def parse2: Tree = {
      pos += 1
      val first = input(pos)
      if (first == '[') {
        val left = parse2
        pos += 1 // ,
        val right = parse2
        pos += 1 // ]
        Pair(left, right)
      } else Number(first.asDigit)
    }
    parse2
  }

  def problem1(input: List[String]): Int = {
    input
      .map(parse)
      .reduceLeft(add)
      .magnitude
  }

  def problem2(input: List[String]): Int = {
    input
      .map(parse)
      .combinations(2)
      .map(a => add(a.head, a.last).magnitude)
      .max
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input18")
    println(problem1(input))
    println(problem2(input))
  }
}
