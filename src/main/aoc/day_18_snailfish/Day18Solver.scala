package aoc.day_18_snailfish

import aoc.file_loader.AOCLoader

sealed trait SnailNumber

case class SnailSingle(number: Int) extends SnailNumber

case class SnailPair(first: SnailNumber, second: SnailNumber) extends SnailNumber

object Day18Solver {

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_18_snailfish")

    val snailNumbers = lines.map(x => parseSnailNumber(x, 0)._1)

    println(solvePart1(snailNumbers))
    println(solvePart2(snailNumbers))
  }

  def makeSnailString(snailNumber: SnailNumber): String = {
    snailNumber match {
      case SnailSingle(n) => n.toString
      case SnailPair(sn1, sn2) => s"[${makeSnailString(sn1)},${makeSnailString(sn2)}]"
    }
  }

  def parseSingle(line: String, start: Int, seek: Boolean): (SnailNumber, Int) = {
    var n1: SnailNumber = SnailSingle(0)
    var next = start
    if (line(next + 1) != '[') {
      n1 = SnailSingle(Integer.parseInt(line(next + 1).toString))
      next = next + 2
    }
    else {
      val (nf, fn) = parseSnailNumber(line, next + 1)
      n1 = nf
      next = fn
    }
    while (seek && next < line.length && line(next) == ']') {
      next += 1
    }
    (n1, next)
  }

  def parseSnailNumber(line: String, start: Int): (SnailNumber, Int) = {
    val (n1, p1) = parseSingle(line, start, seek = false)
    val (n2, p2) = parseSingle(line, p1, seek = true)
    (SnailPair(n1, n2), p2)
  }

  def addLeft(snailNumber: SnailNumber, amount: Int): SnailNumber = {
    snailNumber match {
      case SnailSingle(n) => SnailSingle(n + amount)
      case SnailPair(sn1, sn2) => SnailPair(addLeft(sn1, amount), sn2)
    }
  }

  def addRight(snailNumber: SnailNumber, amount: Int): SnailNumber = {
    snailNumber match {
      case SnailSingle(n) => SnailSingle(n + amount)
      case SnailPair(sn1, sn2) => SnailPair(sn1, addRight(sn2, amount))
    }
  }

  def explodeRecursive(snailNumber: SnailNumber, layer: Int): (SnailNumber, (Int, Int)) = {
    snailNumber match {
      case SnailSingle(_) => (snailNumber, (0, 0))
      case SnailPair(sn1: SnailSingle, sn2: SnailSingle) =>
        if (layer != 4) (snailNumber, (0, 0)) else (SnailSingle(0), (sn1.number, sn2.number))
      case SnailPair(sn1, sn2) =>
        val (snl, (ll, lr)) = explodeRecursive(sn1, layer + 1)
        val (asr, (rl, rr)) = explodeRecursive(addLeft(sn2, lr), layer + 1)
        val asl = addRight(snl, rl)
        (SnailPair(asl, asr), (ll, rr))
    }
  }

  def explode(snailNumber: SnailNumber): SnailNumber = {
    explodeRecursive(snailNumber, 0)._1
  }

  def trySplit(snailNumber: SnailNumber): (SnailNumber, Boolean) = {
    snailNumber match {
      case SnailSingle(n) =>
        if (n > 9)
          (SnailPair(SnailSingle(n / 2), SnailSingle((n + 1) / 2)), true)
        else
          (snailNumber, false)

      case SnailPair(sn1, sn2) =>
        val (snl, hsl) = trySplit(sn1)
        if (hsl) return (SnailPair(snl, sn2), true)
        val (snr, hsr) = trySplit(sn2)
        if (hsr) return (SnailPair(sn1, snr), true)
        (snailNumber, false)

    }
  }

  def normalize(snailNumber: SnailNumber): SnailNumber = {
    var normalized = snailNumber
    var canSplit = true

    while (canSplit) {
      normalized = explode(normalized)
      val (nn, hs) = trySplit(normalized)

      normalized = nn
      canSplit = hs
    }

    normalized
  }

  def magnitude(snailNumber: SnailNumber): BigInt = {
    snailNumber match {
      case SnailSingle(n) => n
      case SnailPair(sn1, sn2) => magnitude(sn1) * 3 + magnitude(sn2) * 2
    }
  }

  def solvePart1(snailNumbers: List[SnailNumber]): BigInt = {
    magnitude(snailNumbers.reduce((sn1, sn2) => normalize(SnailPair(sn1, sn2))))
  }

  def solvePart2(snailNumbers: List[SnailNumber]): BigInt = {
    val a = snailNumbers.combinations(2).map(sns => SnailPair(sns.head, sns(1))).map(normalize).map(magnitude).max
    val b = snailNumbers.combinations(2).map(sns => SnailPair(sns(1), sns.head)).map(normalize).map(magnitude).max
    if (a >= b) a else b
  }
}
