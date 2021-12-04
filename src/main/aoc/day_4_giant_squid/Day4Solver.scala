package aoc.day_4_giant_squid

import aoc.file_loader.AOCLoader

object Day4Solver {

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_4_giant_squid")

    val calledNums = lines.head.split(",").map(_.toInt)
    val grids = lines.drop(1).grouped(6).map(_.drop(1)).map(AOCLoader.toGrid(_, "(?<!^) +")).toList
    val intGrids = grids.toArray.map(_.map(_.map(_.replace(" ", "").toInt).toArray).toArray)

    println(solvePart1(calledNums, intGrids))
    println(solvePart2(calledNums, intGrids))

  }

  def solvePart1(calledNums: Array[Int], intGrids: Array[Array[Array[Int]]]): Int = {
    val newGrids = intGrids.map(_.flatten)
    val markRows = Array.ofDim[Int](intGrids.length, 5)
    val markCols = Array.ofDim[Int](intGrids.length, 5)

    for (i <- calledNums) {
      for (j <- newGrids.indices) {
        val index = newGrids(j).indexOf(i)
        if (index != -1) {
          val row = index / 5
          val col = index % 5

          markRows(j)(row) += 1
          markCols(j)(col) += 1
          newGrids(j)(row * 5 + col) = -1

          if (markRows(j)(row) == 5 || markCols(j)(col) == 5) {
            return (newGrids(j).sum + markRows(j).sum) * i

          }
        }
      }
    }

    0
  }

  def solvePart2(calledNums: Array[Int], intGrids: Array[Array[Array[Int]]]): Int = {
    var newGrids = intGrids.map(_.flatten)
    var markRows = Array.ofDim[Int](intGrids.length, 5)
    var markCols = Array.ofDim[Int](intGrids.length, 5)

    for (i <- calledNums) {
      for (j <- newGrids.indices.reverse) {
        val index = newGrids(j).indexOf(i)
        if (index != -1) {
          val row = index / 5
          val col = index % 5

          markRows(j)(row) += 1
          markCols(j)(col) += 1
          newGrids(j)(row * 5 + col) = -1

          if (markRows(j)(row) == 5 || markCols(j)(col) == 5) {
            if (newGrids.length == 1)
              return (newGrids(j).sum + markRows(j).sum) * i

            newGrids = newGrids.take(j) ++ newGrids.drop(j + 1)
            markRows = markRows.take(j) ++ markRows.drop(j + 1)
            markCols = markCols.take(j) ++ markCols.drop(j + 1)

          }
        }
      }
    }

    0
  }

}
