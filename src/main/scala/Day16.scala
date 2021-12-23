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
  case class Operator(
    version: Int,
    typeId: Long,
    packets: List[Packet]
  ) extends Packet

  def parsePacket(iter: Seq[Char]): (Packet, Seq[Char]) = {
    val version = bToInt(iter.slice(0, 3))
    val typeId = bToInt(iter.slice(3, 6))
    val rest = iter.splitAt(6)._2
    if (typeId == 4) extractLiteralValue(rest, version) else extractSubPackets(rest, version, typeId)
  }

  def extractSubPackets(
    input: Seq[Char],
    version: Int,
    typeId: Int
  ): (Operator, Seq[Char]) = {
    if (input.head == '0') {
      val length = bToInt(input.slice(1, 16))
      val (test, rest) = input.drop(16).splitAt(length)
      var subRest = test
      var packets = List[Packet]()
      while (subRest.nonEmpty) {
        val (packet, rest4) = parsePacket(subRest)
        packets :+= packet
        subRest = rest4
      }
      (Operator(version, typeId, packets), rest)
    } else {
      val numSubPacketsStr = input.slice(1, 12)
      val (packets, rest) = (0 until bToInt(numSubPacketsStr)).foldLeft((List[Packet](), input.drop(12))) {
        case ((packets, input2), _) =>
          val (packet, subRest) = parsePacket(input2)
          (packets :+ packet, subRest)
      }
      (Operator(version, typeId, packets), rest)
    }
  }

  def extractLiteralValue(input: Seq[Char], version: Int): (Literal, Seq[Char]) = {
    val (one, zero) = input.grouped(5).span(_.headOption.getOrElse("") == '1')
    val full = one.toSeq ++ zero.toSeq.take(1)
    (Literal(version, bToLong(full.flatMap(_.tail))), input.drop(full.flatten.length))
  }

  def versionValue(packet: Packet): Long = {
    packet match {
      case Literal(version, _)           => version
      case Operator(version, _, packets) => version + packets.map(versionValue).sum
    }
  }

  def expressionValue(packet: Packet): Long = {
    packet match {
      case Literal(_, value) => value
      case Operator(_, typeId, packets) =>
        (typeId, packets.map(expressionValue)) match {
          case (0, values)              => values.sum
          case (1, values)              => values.product
          case (2, values)              => values.min
          case (3, values)              => values.max
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
