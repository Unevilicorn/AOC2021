package aoc.day_14_extended_polymerization

import aoc.file_loader.AOCLoader


object Day14Solver {

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_14_extended_polymerization")

    val base = lines.head
    val baseCounter = base.zip(base.drop(1)).groupBy(identity)
      .map(p => (p._1, BigInt(p._2.length)))

    val totalCounter = base.groupBy(identity).map(p => (p._1, BigInt(p._2.length)))

    val rules = lines.drop(2).map(_.split(" -> ")).map(p => ((p(0)(0), p(0)(1)), p(1).head)).toMap

    println(solvePart1(rules, baseCounter, totalCounter))
    println(solvePart2(rules, baseCounter, totalCounter))

  }

  def grow(rules: Map[(Char, Char), Char], pCounter: Map[(Char, Char), BigInt], counter: Map[Char, BigInt]): (Map[(Char, Char), BigInt], Map[Char, BigInt]) = {
    var npc = Map[(Char, Char), BigInt]()
    var ntc = counter
    pCounter.foreach {
      case (p@(p1, p2), c) =>
        val newChar = rules.getOrElse(p, ' ')
        npc = npc.updated((p1, newChar), c + npc.getOrElse((p1, newChar), 0))
        npc = npc.updated((newChar, p2), c + npc.getOrElse((newChar, p2), 0))
        ntc = ntc.updated(newChar, c + ntc.getOrElse(newChar,0))
    }

    (npc, ntc)
  }

  def solvePart1(rules: Map[(Char, Char), Char], pc: Map[(Char, Char), BigInt], tc : Map[Char, BigInt]): BigInt = {
    val ocs = List.range(0, 10).foldLeft((pc, tc))((p, _) => grow(rules, p._1, p._2 ))._2.values
    ocs.max - ocs.min
  }


  def solvePart2(rules: Map[(Char, Char), Char], pc: Map[(Char, Char), BigInt], tc : Map[Char, BigInt]): BigInt = {
    val ocs = List.range(0, 40).foldLeft((pc, tc))((p, _) => grow(rules, p._1, p._2 ))._2.values
    ocs.max - ocs.min
  }
}
