package aoc.day_1_sonar_sweep

import aoc.file_loader.AOCLoader

object Day1Solver {
  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_1_sonar_sweep")
    val nums = lines.map(_.toInt)

    solvePart1(nums)
    solvePart2(nums)
  }

  def solvePart1(nums: List[Int]): Unit = {
    var counter = 0
    var prev = nums.head

    nums.drop(1).foreach(x => {
      if (x > prev) counter += 1
      prev = x
    })

    println("Part 1: " + counter)
  }

  def solvePart2(nums: List[Int]): Unit = {
    var counter = 0
    var prev3 = nums.take(3)

    nums.drop(3).foreach(x => {
      if (x > prev3.head) counter += 1
      prev3 = prev3.drop(1) :+ x
    })

    println("Part 2: " + counter)
  }

}
