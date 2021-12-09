package aoc.day_9_smoke_basin

import aoc.file_loader.AOCLoader

import scala.collection.mutable

object Day9Solver {

  private val dirs = Array((0, 1), (0, -1), (1, 0), (-1, 0))

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_9_smoke_basin")

    val stringGrid = AOCLoader.toGrid(lines, "")

    val intGrid = stringGrid.map(_.map(_.toInt).toArray).toArray

    println(solvePart1(intGrid))
    println(solvePart2(intGrid))

  }

  def getWithOutOfRange(x: Int, y: Int, basin: Array[Array[Int]]): Int = {
    val height = basin.length
    val width = basin.head.length
    if (x < 0 || y < 0 || x >= height || y >= width)
      return 10

    basin(x)(y)
  }

  def lowestInArea(x: Int, y: Int, basin: Array[Array[Int]]): Int = {
    val cur = basin(x)(y)

    dirs.foreach(p => {
      if (cur >= getWithOutOfRange(x + p._1, y + p._2, basin))
        return -1
    })

    cur
  }

  def solvePart1(basin: Array[Array[Int]]): Int = {
    val height = basin.length
    val width = basin.head.length
    var sum = 0
    for (x <- 0 until height)
      for (y <- 0 until width) {
        sum += 1 + lowestInArea(x, y, basin)
      }

    sum
  }

  def floodFill(x: Int, y: Int, basin: Array[Array[Int]],
                map: mutable.HashMap[(Int, Int), mutable.Set[(Int, Int)]]): Unit = {
    if (map.contains((x, y)))
      return

    val cur = basin(x)(y)
    val curSet = new mutable.TreeSet[(Int, Int)]()
    curSet.add((x, y))
    map.update((x, y), curSet)

    dirs.foreach(p => {
      val nx = x + p._1
      val ny = y + p._2
      if (cur >= getWithOutOfRange(nx, ny, basin)) {
        floodFill(nx, ny, basin, map)
        map.get((nx, ny)).foreach(_.map(p => {
          curSet.add(p)
          map.update(p, curSet)
        }))
      }
    })
  }

  def solvePart2(basin: Array[Array[Int]]): Int = {
    val map = new mutable.HashMap[(Int, Int), mutable.Set[(Int, Int)]]()
    val height = basin.length
    val width = basin.head.length

    for (x <- 0 until height)
      for (y <- 0 until width)
        if (basin(x)(y) < 9)
          floodFill(x, y, basin, map)

    map.values.toList.distinct.map(_.size).sorted.reverse.take(3).product
  }
}
