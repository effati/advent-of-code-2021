import scala.collection.mutable

object Day06 {

  def problem1(input: List[Int]): Int = {
    var fishes = input
    (0 until 80).foreach { _ =>
      val toAdd = mutable.ListBuffer[Int]()
      fishes = fishes.map(fish => {
        if (fish < 1) {
          toAdd.addOne(8)
          6
        } else {
          fish - 1
        }
      }) ++ toAdd.toList
    }
    fishes.size
  }

  def problem2(input: List[Int]): Long = {
    var fishMap = input.groupBy(identity).map { case (age, fish) => (age, fish.size.toLong) }.toSeq
    (0 until 256).foreach { _ =>
      var toAdd = 0L
      fishMap = fishMap.map {
        case (0, numFishes) =>
          toAdd = numFishes
          (6, numFishes)
        case (age, numFishes) =>
          (age - 1, numFishes)
      } :+ (8, toAdd)
      fishMap = fishMap.groupBy(_._1).map { case (age, seq) => (age, seq.map(_._2).sum) }.toSeq
    }
    BigDecimal(fishMap.map(_._2).sum).toLongExact
  }

  def problem2deque(input: List[Int]): Long = {
    val deque = mutable.ArrayDeque.from(List.fill(9)(0L))
    input
      .groupBy(identity)
      .map { case (age, fish) => (age, fish.size.toLong) }
      .foreach(fish => deque.update(fish._1, fish._2))
    (0 until 256).foreach { _ =>
      val toAdd = deque.head
      deque.append(toAdd).removeHead()
      deque(6) += toAdd
    }
    BigDecimal(deque.sum).toLongExact
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input06").head.split(',').map(_.toInt).toList
    println(problem1(input))
    println(problem2(input))
    println(problem2deque(input))
  }

}
