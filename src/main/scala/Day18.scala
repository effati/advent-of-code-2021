import scala.annotation.tailrec

object Day18 {
  case class Node(n: Int, level: Int)

  def problem1(input: Seq[String]): Int = {
    val nodes = input.map(binTree)
    magnitude(
      nodes.tail.foldLeft(nodes.head) { (acc, node) =>
        println(acc)
        reduce(join(acc, node))
      }
    )
  }

  @tailrec
  def magnitude(nodes: Seq[Node]): Int = {
    if (nodes.length == 1) nodes.head.n
    else {
      val maxLevel = nodes.maxBy(_.level).level
      val (start, rest :: rest2 :: rest3) = nodes.span(_.level != maxLevel)
      magnitude((start :+ Node(3 * rest.n + 2 * rest2.n, maxLevel - 1)) ++ rest3)
    }
  }

  def explode(nodes: Seq[Node], i: Int): Seq[Node] = {
    i match {
      case 0 =>
        val Seq(node1, node2) = nodes.tail.take(2)
        Seq(Node(0, 3), Node(node1.n + node2.n, node2.level)) ++ nodes.drop(3)
      case a if a < nodes.length - 2 =>
        val Seq(node1, node2, node3, node4) = nodes.slice(i - 1, i + 3)
        val mid = Seq(Node(node1.n + node2.n, node1.level), Node(0, 3), Node(node3.n + node4.n, node4.level))
        nodes.take(a - 1) ++ mid ++ nodes.drop(a + 3)
      case _ =>
        val Seq(node1, node2) = nodes.takeRight(2)
        nodes.take(i - 1) ++ Seq(Node(node1.n + node2.n, 3), Node(0, 3))
    }
  }

  def split(nodes: Seq[Node], i: Int): Seq[Node] = {
    val n = nodes(i)
    val (node1, node2) = ((n.n.toDouble / 2).floor.toInt, (n.n.toDouble / 2).ceil.toInt)
    nodes.take(i) ++ Seq(Node(node1, n.level + 1), Node(node2, n.level + 1)) ++ nodes.drop(i + 1)
  }

  def reduce(nodes: Seq[Node]): Seq[Node] = {
    Iterator
      .iterate((nodes, false)) {
        case (nodes, _) =>
          nodes.zipWithIndex.find(_._1.level == 4).map(_._2) match {
            case Some(i) => (explode(nodes, i), false)
            case None =>
              nodes.zipWithIndex.find(_._1.n > 9).map(_._2) match {
                case Some(i) => (split(nodes, i), false)
                case None    => (nodes, true)
              }
          }
      }
      .dropWhile(!_._2)
      .next()
      ._1
  }

  def join(nodes1: Seq[Node], nodes2: Seq[Node]): Seq[Node] = {
    (nodes1 ++ nodes2).map(node => Node(node.n, node.level + 1))
  }

  def binTree(s: String): Seq[Node] = {
    @tailrec
    def tree(acc: Seq[Node], input: Seq[Char], level: Int): Seq[Node] = {
      input match {
        case Nil                          => acc
        case head +: tail if head.isDigit => tree(acc :+ Node(head.asDigit, level), tail, level)
        case '[' +: tail                  => tree(acc, tail, level + 1)
        case ']' +: tail                  => tree(acc, tail, level - 1)
        case _ +: tail                    => tree(acc, tail, level)
      }
    }
    tree(Seq(), s.toSeq, -1)
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input18")
    println(problem1(input))
  }
}
