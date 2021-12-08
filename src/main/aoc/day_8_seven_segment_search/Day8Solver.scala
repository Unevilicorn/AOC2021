package aoc.day_8_seven_segment_search

import aoc.file_loader.AOCLoader

object Day8Solver {

  private val digitSet = Map(
    "abcdef".toSet -> 0,
    "bc".toSet -> 1,
    "abdeg".toSet -> 2,
    "abcdg".toSet -> 3,
    "bcfg".toSet -> 4,
    "acdfg".toSet -> 5,
    "acdefg".toSet -> 6,
    "abc".toSet -> 7,
    "abcdefg".toSet -> 8,
    "abcdfg".toSet -> 9)

  def main(args: Array[String]): Unit = {
        val lines = AOCLoader.loadActual("day_8_seven_segment_search")
//    val lines = AOCLoader.loadExample("day_8_seven_segment_search")

    val pairs = lines.map(_.split(" \\| "))
    val allDigits = pairs.map(p => p(0).split(" "))
    val displayed = pairs.map(p => p(1).split(" "))

    println(solvePart1(displayed))
    println(solvePart2(allDigits, displayed))

  }

  def solvePart1(displayed: List[Array[String]]): Int = {
    displayed.map(_.count(s => (2 <= s.length && s.length <= 4) || s.length == 7)).sum
  }

  def solveSingle(digits: Array[String], displayed: Array[String]): Int = {
    val charCount = digits.flatten.groupBy(identity).map(p => p._1 -> p._2.length)
    var a, b, c, d, e, f, g = ' '

    // Unique occurrences
    c = charCount.find(p => p._2 == 9).head._1
    e = charCount.find(p => p._2 == 4).head._1
    f = charCount.find(p => p._2 == 6).head._1

    // The other segment for 1, with 8 occurrences and not c
    b = digits.find(_.length == 2).head.filter(ch => ch != c).head

    // The other segment with 8 occurrences that's not b
    a = charCount.find(p => p._2 == 8 && p._1 != b).head._1

    // Difference between 1 and 4 that's not f
    g = digits.find(_.length == 4).head.diff(digits.find(_.length == 2).head).filter(_ != f).head

    // The other segment with 7 occurrences that's not g
    d = charCount.find(p => p._2 == 7 && p._1 != g).head._1

    val conversion = Map(a -> 'a', b -> 'b', c -> 'c', d -> 'd', e -> 'e', f -> 'f', g -> 'g')
    displayed.map(_.map(ch => conversion(ch)))
      .foldLeft(0)((a, s) => a * 10 + digitSet(s.sorted.toSet))
  }

  def solvePart2(allDigits: List[Array[String]], displayed: List[Array[String]]): Int = {
    allDigits.zip(displayed).map(p => solveSingle(p._1, p._2)).sum
  }
}
