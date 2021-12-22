object Day16 {
  val Conversions = Map('0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  val sample = "C0015000016115A2E0802F182340"

  val bToInt: Seq[Char] => Int = a => Integer.parseInt(a.mkString, 2)
  val bToLong: Seq[Char] => Long = a => java.lang.Long.parseLong(a.mkString, 2)


  def extractSubPackets(iter: Seq[Char]): (Int, Seq[Char]) = {
    val (lengthTypeId, rest) = iter.splitAt(1)
//    println("length type: " + lengthTypeId.head)
    if (lengthTypeId.head == '0') {
      val (lengthStr, rest2) = rest.splitAt(15)
      val length = bToInt(lengthStr)
//      println("subpacket length: " + length)
      val (test, rest3) = rest2.splitAt(length)
      var subRest = test
      var version = 0
      while (subRest.nonEmpty) {
        val a = parsePacket(subRest)
        version += a._1
        subRest = a._2
      }
      (version, rest3)
    } else {
      val (numSubPacketsStr, rest2) = rest.splitAt(11)
      val numSubPackets = bToInt(numSubPacketsStr)
//      println("num subpackets: " + numSubPackets)
      (0 until numSubPackets).foldLeft((0, rest2)) {case ((sum, iter2), _) =>
        val (value, newIter) = parsePacket(iter2)
        (sum + value, newIter)
      }
    }
  }

  def parsePacket(iter: Seq[Char]): (Int, Seq[Char]) = {
//    println("----")
    val (versionStr, rest) = iter.splitAt(3)
    val version = bToInt(versionStr)
//    println("version: " + version)
    val (typeIdStr, rest2) = rest.splitAt(3)
    val typeId = bToInt(typeIdStr)
//    println("type: " + typeId)
    val isLiteralValue = if (typeId == 4) true else false
    val (value, rest3) = if (isLiteralValue) extractLiteralValue(rest2) else extractSubPackets(rest2)
    (value + version, rest3)
  }

  def problem1(bits: String): Int = {
    parsePacket(bits)._1
  }

  def extractLiteralValue(iter: Seq[Char]): (Int, Seq[Char]) = {
    var literalValue = Seq[Char]()
    var lastFound = false
    var finalRest = iter
    while (!lastFound) {
      val (first, rest3) = finalRest.splitAt(1)
      if (first.head == '0') lastFound = true
      val newV = rest3.splitAt(4)
      literalValue ++= newV._1
      finalRest = newV._2
    }
//    println("literal value: " + bToLong(literalValue))
    val mod = literalValue.length % 4
    val padded = if (mod == 0) 0 else 4 - mod
    (0, finalRest.drop(padded))
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input16").head
//    val input = sample
    val bits = input.map(Conversions).mkString
//    println(bits)
    println(problem1(bits))

  }

}
