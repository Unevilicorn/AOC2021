package aoc.day_6_lantern_fish

import aoc.file_loader.AOCLoader

import scala.collection.mutable

object Day6Solver {

  private val hashMap = new mutable.HashMap[(Int, Int), BigInt]()

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_6_lantern_fish")

    val timers = lines.head.split(",").map(_.toInt)

    presetHashmap(114514)

    println(timers.map(t => recurrent(80, t)).sum)
    println(timers.map(t => recurrent(256, t)).sum)
    println(timers.map(t => recurrent(114514, t)).sum)
  }

  def presetHashmap(time: Int): Unit = {
    for (t <- 0 to time){
      List(6, 8).foreach(c => hashMap.getOrElseUpdate((t, c), {
        if (t >= c + 1)
          hashMap.getOrElse((t - c - 1, 6), 1:BigInt) + hashMap.getOrElse((t - c - 1, 8), 1:BigInt)
        else
          1
      }))
    }
  }

  def recurrent(time: Int, offset: Int): BigInt = {
    hashMap.getOrElseUpdate((time, offset), {
      if (time < offset + 1) return 1
      recurrent(time - offset - 1, 6) + recurrent(time - offset - 1, 8)
    })
  }
}
