package aoc.day_7_treachery_of_whales

import aoc.file_loader.AOCLoader

import scala.collection.mutable

object Day7Solver {

  private val hashMap = new mutable.HashMap[(Int, Int), BigInt]()

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_7_treachery_of_whales")

    val positions = lines.head.split(",").map(_.toInt)

    println(solvePart1(positions))
    println(solvePart2(positions))

  }

  def solvePart1(position: Array[Int]): Int = {
    position.map(h => position.map(_ - h).map(Math.abs).sum).min
  }

  def solvePart2(position: Array[Int]): Int = {
    List.range(position.min, position.max + 1)
      .map(h => position.map(_ - h).map(Math.abs).map(n => n * (n + 1) / 2).sum).min
  }
}
