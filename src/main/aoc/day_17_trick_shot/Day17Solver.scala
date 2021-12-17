package aoc.day_17_trick_shot

import aoc.file_loader.AOCLoader

case class Pos(x: Int, y: Int)

case class Rect(p1: Pos, p2: Pos)

object Day17Solver {

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_17_trick_shot")
    //    val lines = AOCLoader.loadExample("day_17_trick_shot")

    val stringPos = lines.head.split(": ")(1).split(", ").map(_.split("=")(1).split("\\.\\."))
    val intPos = stringPos.map(_.map(_.toInt))
    val target = Rect(Pos(intPos(0)(0), intPos(1)(1)), Pos(intPos(0)(1), intPos(1)(0)))
    println(target)


    println(solvePart1(target))
    println(solvePart2(target))
  }

  def sumRange(n: Int): Int = {
    n * (n + 1) / 2
  }

  def solvePart1(target: Rect): BigInt = {
    sumRange(target.p2.y)
  }

  def willHit(target: Rect, vel: Pos): Boolean = {
    var vx = vel.x
    var vy = vel.y
    var px, py = 0
    while (px <= target.p2.x && py >= target.p2.y) {
      if (px >= target.p1.x && py <= target.p1.y)
        return true
      px += vx
      py += vy
      vx = Math.max(vx - 1, 0)
      vy -= 1
    }

    false
  }

  def solvePart2(target: Rect): BigInt = {
    var count = 0
    for (x <- 0 to target.p2.x) {
      for (y <- target.p2.y to Math.abs(target.p2.y)) {
        if (willHit(target, Pos(x, y)))
          count += 1
      }
    }
    count
  }
}
