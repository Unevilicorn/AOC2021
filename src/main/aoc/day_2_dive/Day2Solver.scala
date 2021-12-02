package aoc.day_2_dive

import aoc.file_loader.AOCLoader

object Day2Solver {

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_2_dive")
    val pairs = AOCLoader.toGrid(lines, " ")

    val stringNumPair = pairs.map(pair => (pair.head, pair(1).toInt))

    solvePart1(stringNumPair)
    solvePart2(stringNumPair)
  }

  def solvePart1(stringNumPair: List[(String, Int)]): Unit = {
    val dirMapping = Map(("forward", (1, 0)), ("down", (0, 1)), ("up", (0, -1)))

    val velocities = stringNumPair.flatMap(
      p => dirMapping.get(p._1).map(
        d => (d._1 * p._2, d._2 * p._2)
      ))

    val total = velocities.foldLeft((0, 0))((a, b) => (a._1 + b._1, a._2 + b._2))
    val product = total._1 * total._2

    println("Part 1: " + product)
  }

  def solvePart2(stringNumPair: List[(String, Int)]): Unit = {
    var aim = 0
    var pos = (0, 0)

    stringNumPair.foreach(p => p._1 match {
      case "down" => aim += p._2
      case "up" => aim -= p._2
      case "forward" => pos = (pos._1 + p._2, pos._2 + aim * p._2)
    })

    val product = pos._1 * pos._2

    println("Part 2: " + product)
  }

}
