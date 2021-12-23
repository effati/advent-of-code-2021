object Day16 {
  val Conversions = Map(
    '0' -> "0000",
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

  val bToInt: Seq[Char] => Int = a => Integer.parseInt(a.mkString, 2)
  val bToLong: Seq[Char] => Long = a => java.lang.Long.parseLong(a.mkString, 2)

  sealed trait Packet
  case class Literal(version: Int, value: Long) extends Packet
  case class Operator(version: Int, typeId: Long, packets: List[Packet]) extends Packet

  def extractSubPackets(iter: Seq[Char]): (List[Packet], Seq[Char]) = {
    val (lengthTypeId, rest) = iter.splitAt(1)
    if (lengthTypeId.head == '0') {
      val (lengthStr, rest2) = rest.splitAt(15)
      val length = bToInt(lengthStr)
      val (test, rest3) = rest2.splitAt(length)
      var subRest = test
      var packets = List[Packet]()
      while (subRest.nonEmpty) {
        val a = parsePacket(subRest)
        packets :+= a._1
        subRest = a._2
      }
      (packets, rest3)
    } else {
      val (numSubPacketsStr, rest2) = rest.splitAt(11)
      val numSubPackets = bToInt(numSubPacketsStr)
      (0 until numSubPackets).foldLeft((List[Packet](), rest2)) {
        case ((packets, iter2), _) =>
          val (packet, newIter) = parsePacket(iter2)
          (packets :+ packet, newIter)
      }
    }
  }

  def parsePacket(iter: Seq[Char]): (Packet, Seq[Char]) = {
    val (versionStr, rest) = iter.splitAt(3)
    val version = bToInt(versionStr)
    val (typeIdStr, rest2) = rest.splitAt(3)
    val typeId = bToInt(typeIdStr)
    val isLiteralValue = if (typeId == 4) true else false
    val (valu2e, rest3) = if (isLiteralValue) {
      val (value, rest4) = extractLiteralValue(rest2)
      (Literal(version, value), rest4)
    } else {
      val (value, rest4) = extractSubPackets(rest2)
      (Operator(version, typeId, value), rest4)
    }
    (valu2e, rest3)
  }

  def extractLiteralValue(iter: Seq[Char]): (Long, Seq[Char]) = {
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
    val mod = literalValue.length % 4
    val padded = if (mod == 0) 0 else 4 - mod
    (bToLong(literalValue), finalRest.drop(padded))
  }

  def versionValue(packet: Packet): Long = {
    packet match {
      case Literal(version, _) => version
      case Operator(version, _, packets) => version + packets.map(versionValue).sum
    }
  }

  def expressionValue(packet: Packet): Long = {
    packet match {
      case Literal(_, value) => value
      case Operator(_, typeId, packets) => (typeId, packets.map(expressionValue)) match {
        case (0, values) => values.sum
        case (1, values) => values.product
        case (2, values) => values.min
        case (3, values) => values.max
        case (5, List(first, second)) => if (first > second) 1L else 0L
        case (6, List(first, second)) => if (first < second) 1L else 0L
        case (7, List(first, second)) => if (first == second) 1L else 0L
      }
    }
  }

  def problem1(bits: String): Long = versionValue(parsePacket(bits)._1)
  def problem2(bits: String): Long = expressionValue(parsePacket(bits)._1)

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input16").head
    val bits = input.map(Conversions).mkString
    println(problem1(bits))
    println(problem2(bits))
  }
}
