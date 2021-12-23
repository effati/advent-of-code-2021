object Day16 {
  sealed trait Packet
  case class Literal(version: Int, value: Long) extends Packet
  case class Operator(version: Int, typeId: Long, packets: List[Packet]) extends Packet

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

  val binInt: Seq[Char] => Int = b => Integer.parseInt(b.mkString, 2)
  val binLong: Seq[Char] => Long = b => java.lang.Long.parseLong(b.mkString, 2)

  def parsePacket(input: Seq[Char]): (Packet, Seq[Char]) = {
    val version = binInt(input.slice(0, 3))
    val typeId = binInt(input.slice(3, 6))
    val rest = input.splitAt(6)._2
    if (typeId == 4) extractLiteralValue(rest, version)
    else if (rest.head == '0') extractLengthBits(rest.tail, version, typeId)
    else extractNumPackets(rest.tail, version, typeId)
  }

  def extractLiteralValue(input: Seq[Char], version: Int): (Literal, Seq[Char]) = {
    val (one, zero) = input.grouped(5).span(_.headOption.getOrElse("") == '1')
    val full = (one ++ zero.take(1)).toSeq
    (Literal(version, binLong(full.flatMap(_.tail))), input.drop(full.flatten.length))
  }

  def extractLengthBits(input: Seq[Char], version: Int, typeId: Int): (Packet, Seq[Char]) = {
    var (bits, rest) = input.drop(15).splitAt(binInt(input.slice(0, 15)))
    var packets = List[Packet]()
    while (bits.nonEmpty) {
      val (packet, rest4) = parsePacket(bits)
      packets :+= packet
      bits = rest4
    }
    (Operator(version, typeId, packets), rest)
  }

  def extractNumPackets(input: Seq[Char], version: Int, typeId: Int): (Operator, Seq[Char]) = {
    val (packets, rest) = (0 until binInt(input.slice(0, 11))).foldLeft((List[Packet](), input.drop(11))) {
      case ((packets, rest), _) =>
        val (packet, subRest) = parsePacket(rest)
        (packets :+ packet, subRest)
    }
    (Operator(version, typeId, packets), rest)
  }

  def versionValue(packet: Packet): Long = packet match {
    case Literal(version, _)           => version
    case Operator(version, _, packets) => version + packets.map(versionValue).sum
  }

  def expressionValue(packet: Packet): Long = packet match {
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

  def problem1(bits: String): Long = versionValue(parsePacket(bits)._1)
  def problem2(bits: String): Long = expressionValue(parsePacket(bits)._1)

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input16").head.map(Conversions).mkString
    println(problem1(input))
    println(problem2(input))
  }
}
