object Day14 {
  type Rules = Map[(Char, Char), Char]
  type CharCount = Map[Char, Long]
  type PairCount = Map[(Char, Char), Long]

  def pairInsertion(
    rules: Rules,
    initCharCount: CharCount,
    initPairCount: PairCount,
    steps: Int
  ): Long = {
    val (count, _) = (0 until steps)
      .foldLeft((initCharCount, initPairCount)) {
        case ((charCount, pairCount), _) =>
          step(rules, charCount, pairCount)
      }
    val vals = count.values.toList
    vals.max - vals.min
  }

  def step(
    rules: Rules,
    charCount: CharCount,
    pairCount: PairCount
  ): (CharCount, PairCount) = {
    pairCount.foldLeft((charCount, Map[(Char, Char), Long]())) {
      case ((charCount2, pairCount2), (pair, count)) =>
        val (a, b) = pair
        val insertion = rules(pair)

        (charCount2 + (insertion -> (charCount2.getOrElse(insertion, 0L) + count))) -> (pairCount2 ++ Map(
          (a, insertion) -> (count + pairCount2.getOrElse((a, insertion), 0L)),
          (insertion, b) -> (count + pairCount2.getOrElse((insertion, b), 0L))
        ))
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input14")
    val initCharCount = input.head.groupBy(identity).map { case (mol, mols) => mol -> mols.length.toLong }
    val initPairCount = input.head
      .sliding(2)
      .map(_.toList)
      .map { a => (a.head, a.last) }
      .toList
      .groupBy(identity)
      .map(a => (a._1, a._2.length.toLong))

    val rules = input.tail
      .filterNot(_.isBlank)
      .map(_.split(" -> "))
      .map(rule => (rule.head.head, rule.head.last) -> rule.last.head)
      .toMap

    println(pairInsertion(rules, initCharCount, initPairCount, 10))
    println(pairInsertion(rules, initCharCount, initPairCount, 40))
  }
}
