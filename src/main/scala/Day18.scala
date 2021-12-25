import scala.annotation.tailrec

object Day18 {
  sealed trait Node {
    def explode(depth: Int): ExplodeResult
    def split: Node
    def magnitude: Int
    def addLeft(value: Int): Node
    def addRight(value: Int): Node
  }
  case class Pair(left: Node, right: Node) extends Node {
    override def explode(depth: Int): ExplodeResult = {
      (left, right) match {
        case (Number(l), Number(r)) if depth <= 0 => ExplodeResult(Number(0), l, r)
        case _ =>
          val leftExp = left.explode(depth - 1)
          val nr = if (leftExp.addRight != 0) right.addLeft(leftExp.addRight) else right
          val rightExp = nr.explode(depth - 1)
          val nl = if (rightExp.addLeft != 0) leftExp.node.addRight(rightExp.addLeft) else leftExp.node
          ExplodeResult(Pair(nl, rightExp.node), leftExp.addLeft, rightExp.addRight)
      }
    }

    override def split: Node = {
      val nl = left.split
      if (nl != left) Pair(nl, right) else Pair(left, right.split)
    }
    override def magnitude: Int = 3 * left.magnitude + 2 * right.magnitude
    override def addLeft(value: Int): Node = Pair(left.addLeft(value), right)
    override def addRight(value: Int): Node = Pair(left, right.addRight(value))
  }
  case class ExplodeResult(node: Node, addLeft: Int, addRight: Int)

  case class Number(value: Int) extends Node {
    def explode(depth: Int): ExplodeResult = ExplodeResult(this, 0, 0)
    def split: Node = {
      if (value >= 10) {
        val newLeft = value / 2
        Pair(Number(newLeft), Number(value - newLeft))
      }
      else Number(value)
    }
    def magnitude: Int = value
    def addLeft(value2: Int): Node = Number(value + value2)
    def addRight(value2: Int): Node = Number(value + value2)
  }

  def problem1(input: List[String]): Int = {
    input
      .map(parse)
      .reduceLeft(add)
      .magnitude
  }

  private def add(a: Node, b: Node): Node = reduce(Pair(a, b))

  @tailrec
  private def reduce(node: Node): Node = {
    val next = node.explode(4).node.split
    if (next == node) next
    else reduce(next)
  }

  def parse(input: String): Node = {
    var pos = -1
    def parse2: Node = {
      pos += 1
      val first = input(pos)
      if (first == '[') {
        val left = parse2
        pos += 1 // ,
        val right = parse2
        pos += 1 // ]
        Pair(left, right)
      }
      else Number(first.asDigit)
    }
    parse2
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input18")
    println(problem1(input))
  }
}
