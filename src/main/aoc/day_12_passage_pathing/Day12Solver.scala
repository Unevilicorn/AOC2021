package aoc.day_12_passage_pathing

import aoc.file_loader.AOCLoader

case class Pair(from: String, to: String)

object Day12Solver {

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_12_passage_pathing")
    // val lines = AOCLoader.loadExample("day_12_passage_pathing")

    val pairs = lines.map(_.split("-"))
      .flatMap(a => List(Pair(a(0), a(1)), Pair(a(1), a(0))))
      .filterNot(_.to == "start")
      .filterNot(_.from == "end")

    val graph = pairs.groupBy(_.from).view.mapValues(_.map(_.to)).toMap

    println(solvePart1(graph))
    println(solvePart2(graph))

  }

  def findPathRecursive(graph: Map[String, List[String]], from: String, to: String,
                        reenter: Boolean, current: List[String]): List[List[String]] = {
    if (from == to)
      return List(current :+ to)

    val canVisitMultiple = from == from.toUpperCase()

    val newCurrent = from +: current

    val noB = graph.getOrElse(from, List.empty)
      .flatMap(f => findPathRecursive(
        graph.filter(_._1 != from || canVisitMultiple),
        f, to, reenter, newCurrent))

    var withB = List[List[String]]()
    if (reenter && !canVisitMultiple && from != "start") {
      withB = graph.getOrElse(from, List.empty)
        .flatMap(f => findPathRecursive(
          graph, f, to, reenter = false, newCurrent))
    }

    noB ::: withB
  }


  def solvePart1(graph: Map[String, List[String]]): Int = {
    findPathRecursive(graph, "start", "end", reenter = false, current = List()).distinct.length
  }

  def solvePart2(graph: Map[String, List[String]]): Int = {
    findPathRecursive(graph, "start", "end", reenter = true, current = List()).distinct.length
  }
}
