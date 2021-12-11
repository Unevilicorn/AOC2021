package aoc.day_11_dumbo_octopus

import aoc.file_loader.AOCLoader

object Day11Solver {

  def main(args: Array[String]): Unit = {
        val lines = AOCLoader.loadActual("day_11_dumbo_octopus")
//    val lines = AOCLoader.loadExample("day_11_dumbo_octopus")
    val grid = AOCLoader.toGrid(lines, "")

    println(solvePart1(grid.map(_.map(_.toInt * 2).toArray).toArray))
    println(solvePart2(grid.map(_.map(_.toInt * 2).toArray).toArray))

  }

  def inRange(x: Int, y: Int): Boolean = {
    !(x < 0 || x > 9 || y < 0 || y > 9)
  }

  def increment(x: Int, y: Int, intGrid: Array[Array[Int]]): Int = {
    if (inRange(x, y)) {
      intGrid(x)(y) += 2

      if (intGrid(x)(y) >= 20 && intGrid(x)(y) % 2 == 0) {
        intGrid(x)(y) += 1
        return aoeIncrement(x, y, intGrid) + 1
      }
    }
    0
  }

  def aoeIncrement(x: Int, y: Int, intGrid: Array[Array[Int]]): Int = {
    var counter = 0
    for (i <- -1 to 1)
      for (j <- -1 to 1)
        counter += increment(x + i, y + j, intGrid)
    counter
  }

  def step(intGrid: Array[Array[Int]]): Int = {
    var counter = 0
    for (i <- 0 until 10)
      for (j <- 0 until 10)
        counter += increment(i, j, intGrid)

    for (i <- 0 until 10)
      for (j <- 0 until 10)
        if(intGrid(i)(j) >= 20) {
          intGrid(i)(j) = 0
        }
    counter
  }

  def solvePart1(intGrid: Array[Array[Int]]): Int = {
    var counter = 0
    for (_ <- 1 to 100)
      counter += step(intGrid)

    counter
  }

  def solvePart2(intGrid: Array[Array[Int]]): Int = {
    for (i <- 1 to 100000000)
      {
        step(intGrid)
        if(intGrid.map(_.count(_ == 0)).sum == 100)
          return i
      }

    -1
  }
}
