package aoc.day_10_syntax_scoring

import aoc.file_loader.AOCLoader

import scala.collection.mutable

object Day10Solver {

  val valueTable: Map[Char, BigInt] = Map[Char, BigInt]((')', 3), (']', 57), ('}', 1197), ('>', 25137))
  val valMap2: Map[Char, BigInt] = Map[Char, BigInt]((')', 1), (']', 2), ('}', 3), ('>', 4))
  val openMap: Map[Char, Char] = Map[Char, Char](('(', ')'), ('[', ']'), ('{', '}'), ('<', '>'))

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_10_syntax_scoring")

    println(solvePart1(lines))
    println(solvePart2(lines))

  }

  def firstMissing(line: String, stack: mutable.Stack[Char]): BigInt = {
    for (c <- line) {
      if (openMap.contains(c)) {
        stack.push(c)
      } else if (openMap(stack.pop()) != c) {
        return valueTable(c)
      }
    }
    0
  }

  def calcMissing(line: String): BigInt = {
    val stack = mutable.Stack[Char]()
    if (firstMissing(line, stack) == 0){
      return stack.toList.map(c => valMap2(openMap(c))).foldLeft(0:BigInt)((a, x) => a * 5 + x)
    }
    0
  }

  def solvePart1(lines: List[String]): BigInt = {
    lines.map(firstMissing(_, mutable.Stack[Char]())).sum
  }

  def solvePart2(lines: List[String]): BigInt = {
    val a = lines.map(calcMissing).filter(_ != 0).sorted
    a(a.size / 2)
  }
}
