package aoc.day_15_chiton

import aoc.file_loader.AOCLoader

import scala.collection.mutable

case class Coord(r: Int, c: Int)

case class PQPair(coord: Coord, weight: Int)

object Day15Solver {

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_15_chiton")

    val intGrid = AOCLoader.toGrid(lines, "").map(_.map(_.toInt))

    println(solvePart1(intGrid))
    println(solvePart2(intGrid))
  }

  def inRange(coord: Coord, size: Coord): Boolean = {
    !(coord.r < 0 || coord.r >= size.r || coord.c < 0 || coord.c >= size.c)
  }

  def intGridGet(intGrid: List[List[Int]], coord: Coord): Int = {
    val size = Coord(intGrid.length, intGrid.head.length)

    if (inRange(coord, size))
      intGrid(coord.r)(coord.c)
    else
      -1
  }

  def findShortest(intGrid: List[List[Int]]): Int = {
    val size = Coord(intGrid.length, intGrid.head.length)
    val start = Coord(0, 0)
    val end = Coord(size.r - 1, size.c - 1)

    val visited = new mutable.HashMap[Coord, Boolean]().withDefaultValue(false)

    val pq = new mutable.PriorityQueue[PQPair]()(Ordering.by[PQPair, Int](-_.weight))
    pq.enqueue(PQPair(start, 0))


    while (pq.nonEmpty) {
      val current = pq.dequeue()
      val coord = current.coord

      if (coord == end)
        return current.weight

      if (inRange(coord, size) && intGridGet(intGrid, coord) != -1 && !visited(coord)) {
        visited(coord) = true
        pq.addOne(PQPair(Coord(coord.r - 1, coord.c), current.weight + intGridGet(intGrid, Coord(coord.r - 1, coord.c))))
        pq.addOne(PQPair(Coord(coord.r + 1, coord.c), current.weight + intGridGet(intGrid, Coord(coord.r + 1, coord.c))))
        pq.addOne(PQPair(Coord(coord.r, coord.c - 1), current.weight + intGridGet(intGrid, Coord(coord.r, coord.c - 1))))
        pq.addOne(PQPair(Coord(coord.r, coord.c + 1), current.weight + intGridGet(intGrid, Coord(coord.r, coord.c + 1))))
      }
    }
    -1
  }

  def incMod(x: Int): Int = {
    x % 9 + 1
  }

  def solvePart1(intGrid: List[List[Int]]): Int = {
    findShortest(intGrid)
  }

  def solvePart2(intGrid: List[List[Int]]): Int = {
    val intGridx5 = (1 until 5).scanLeft(intGrid)((a, _) => a.map(_.map(incMod))).flatten

    val intGridx5x5 = (1 until 5).scanLeft(intGridx5)((a, _) => a.map(_.map(incMod)))
      .reduce((a, b) => a.zip(b).map(p => p._1 ++ p._2)).toList

    findShortest(intGridx5x5)
  }
}
