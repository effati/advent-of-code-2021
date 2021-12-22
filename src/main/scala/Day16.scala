object Day16 {
  val sample = "8A004A801A8002F478"

  val bToInt: Seq[Char] => Int = a => Integer.parseInt(a.mkString, 2)

  def extractSubPackets(iter: Iterator[Char]): (Int, Iterator[Char]) = {
    val lengthTypeId = iter.next()
    if (lengthTypeId == '0') {
      val length = bToInt((0 until 15).map(_ => iter.next()))
      val test = (0 until length).map(_ => iter.next()).iterator
      parsePacket(test)
    } else {
      val numSubPackets = bToInt((0 until 11).map(_ => iter.next()))
      (0 until numSubPackets).foldLeft((0, iter)) {case ((sum, iter2), _) =>
        val (value, newIter) = parsePacket(iter2)
        (sum + value, newIter)
      }
    }
  }

  def parsePacket(iter: Iterator[Char]): (Int, Iterator[Char]) = {
    val version = bToInt((0 until 3).map(_ => iter.next()))
    val typeId = bToInt((0 until 3).map(_ => iter.next()))
    val isLiteralValue = if (typeId == 4) true else false
    val (value, newIter) = if (isLiteralValue) extractLiteralValue(iter) else extractSubPackets(iter)
    (value + version, newIter)
  }

  def problem1(bits: String): Int = {
    val iter = bits.iterator
    parsePacket(iter)._1
  }

  def extractLiteralValue(iter: Iterator[Char]): (Int, Iterator[Char]) = {
    var literalValue = List[String]()
    var lastFound = false
    while (!lastFound && iter.hasNext) {
      val first = iter.next()
      if (first == '0') lastFound = true
      literalValue = literalValue :+ iter.take(4).mkString
    }
//    (bToInt(literalValue.mkString.iterator), iter.dropWhile(_ == '0'))
    (0, iter.dropWhile(_ == '0'))
  }

  def main(args: Array[String]): Unit = {
//    val input = Utils.read("input16").head
    val input = sample
    val bits = BigInt(input, 16).toString(2)
    println(problem1(bits))

  }

}
