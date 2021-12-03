package aoc.day_3_binary_diagnostic

import aoc.file_loader.AOCLoader

object Day3Solver {

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_3_binary_diagnostic")
    //    val lines = AOCLoader.loadExample("day_3_binary_diagnostic")
    val intGrid = AOCLoader.toGrid(lines, "").map(_.map(_.toInt))

    solvePart1(intGrid)
    solvePart2(intGrid)
  }

  def solvePart1(intGrid: List[List[Int]]): Unit = {

    val directionalGrid = intGrid.map(_.map(x => x * 2 - 1))
    val sumDirectionalGrid = directionalGrid.reduce((a, b) => a.zip(b).map(x => x._1 + x._2))

    val mostGrid = sumDirectionalGrid.map(x => x > 0)
    val gamma = mostGrid.foldLeft(0)((n, b) => n * 2 + (if (b) 1 else 0))

    val leastGrid = sumDirectionalGrid.map(x => x <= 0)
    val epsilon = leastGrid.foldLeft(0)((n, b) => n * 2 + (if (b) 1 else 0))

    println("Part 1: " + gamma * epsilon)
  }

  def solvePart2(intGrid: List[List[Int]]): Unit = {
    var mostIntGrid = intGrid.map(_.toArray)
    var leastIntGrid = intGrid.map(_.toArray)

    val len = leastIntGrid.head.length

    var i = 0
    while (i < len) {
      var most = 1
      mostIntGrid.foreach(a => most += (a(i) * 2 - 1))
      most = if (most >= 1) 1 else 0
      mostIntGrid = mostIntGrid.filter(_ (i) == most)
      i += 1
    }

    i = 0
    while (i < len) {
      var least = 0
      leastIntGrid.foreach(a => least += (a(i) * 2 - 1))
      least = if (least >= 0) 0 else 1
      leastIntGrid = leastIntGrid.filter(_ (i) == least)

      if (leastIntGrid.length == 1)
        i = len + 1

      i += 1
    }

    val o = mostIntGrid.head.foldLeft(0)((n, b) => n * 2 + b)
    val c = leastIntGrid.head.foldLeft(0)((n, b) => n * 2 + b)
    println("Part 2: " + o * c)
  }

}
