package aoc.day_6_lantern_fish

import aoc.file_loader.AOCLoader

import scala.collection.mutable

object Day6Solver {

  private val hashMap = new mutable.HashMap[(Int, Int), Long]()

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_6_lantern_fish")

    val timers = lines.head.split(",").map(_.toInt)

    println(solvePart1(timers))
    println(solvePart2(timers))

  }

  def recurrent(time: Int, offset: Int): Long = {
    hashMap.getOrElseUpdate((time, offset), {
      if (time < offset + 1) return 1
      1 + List.range(0, Math.ceil((time - offset - 1) / 7.0).toInt)
        .map(i => recurrent((time - offset - 1) - 7 * i, 8)).sum
    })
  }

  def solvePart1(timers: Array[Int]): Long = {
    timers.map(t => recurrent(81, t)).sum
  }

  def solvePart2(timers: Array[Int]): Long = {
    timers.map(t => recurrent(257, t)).sum
  }

}
