package aoc.day_5_hydrothermal_venture

import aoc.file_loader.AOCLoader

import scala.collection.mutable

object Day5Solver {

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_5_hydrothermal_venture")

    val splitLine = lines.map(_.split(" -> ").flatMap(_.split(",").map(_.toInt)))
    val realLines = splitLine.map(p => new Line(p(0), p(1), p(2), p(3)))

    println(solvePart1(realLines))
    println(solvePart2(realLines))

  }

  def generalSolver(lines :List[Line]) : Int = {
    val hm = new mutable.HashMap[String, Int].withDefaultValue(0)

    lines.foreach(
      _.occupiedPoints.foreach(point => {
        hm.update(point.toString, hm(point.toString) + 1)
      })
    )

    hm.count(p => p._2 >= 2)
  }

  def solvePart1(lines :List[Line]): Int = {
    generalSolver(lines.filter(_.notDiagonal))
  }

  def solvePart2(lines :List[Line]): Int = {
    generalSolver(lines)
  }

}
