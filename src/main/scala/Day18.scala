import scala.annotation.tailrec

object Day18 {
  sealed trait Tree
//  case class Node(left: Tree, right: Tree) extends Tree
  case class Leaf(value: Int) extends Tree

  val PTree = "\\[(.+)]".r
  val PTuple = "(\\d),(\\d)".r
  val PTuples = "(\\[.+]),(\\[.+])".r
  val PLeft = "(\\d),(.+)".r
  val PRight = "(.+),(\\d)".r
  val PLeaf = "(\\d)".r

  case class Node(n: Int, level: Int)

  def binTree2(nodes: List[Node]): Tree = {
    def test(nodes2: List[Node], targetLevel: Int): Tree = {
      if (targetLevel == 0) {
        
      }
    }
  }

  def binTree(s: String): List[Node] = {
    @tailrec
    def helper(xs: List[Char], level: Int, acc: List[Node]): List[Node] = xs match {
      case Nil => acc
      case head :: tail if head.isDigit => helper(tail, level, acc :+ Node(head.asDigit, level))
      case '[' :: tail => helper(tail, level + 1, acc)
      case ']' :: tail => helper(tail, level - 1, acc)
      case _ :: tail => helper(tail, level, acc)
    }
    helper(s.toList, -1, Nil)
  }

/*  def binTree(input: List[Char], stack: List[Char]): Tree = {
    val head :: tail = input
    if (head == '[') binTree(tail, head :: stack)
    else if (head == ']') Node(binTree(stack, List()), binTree(tail, List()))
    else {
      Node(Leaf(head.toInt), binTree(tail.tail, )
    }
  }*/

 /* def binTree(input: String): Tree = {
    input match {
      case PTuples(left, right) => Node(binTree(left), binTree(right))
      case PTree(tree) => binTree(tree)
      case PTuple(left, right) => Node(binTree(left), binTree(right))
      case PLeft(left, right) => Node(Leaf(left.toInt), binTree(right))
      case PRight(left, right) => Node(binTree(left), Leaf(right.toInt))
      case _ => {
        println("wtf " + input)
        Leaf(-1)
      }
    }
  }*/

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input18")
    input.map(binTree).foreach(println)

//    println(problem1(input))
  }
}
