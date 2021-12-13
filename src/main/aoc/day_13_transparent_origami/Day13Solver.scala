package aoc.day_13_transparent_origami

import aoc.file_loader.AOCLoader

object Day13Solver {

  case class Coord(x: Int, y: Int)
  case class Paper(size: Coord, points: List[Coord])
  case class FoldDir(dir: String, pos: Int)

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_13_transparent_origami")

    val coordinates = lines.takeWhile(_ != "")
      .map(_.split(",")).map(a => Coord(a(0).toInt, a(1).toInt))

    val instructions = lines.dropWhile(_ != "")
      .drop(1).map(_.split(" ")).map(_ (2)).map(_.split("="))
      .map(f => FoldDir(f(0), f(1).toInt))

    println(solvePart1(coordinates, instructions))
    println(solvePart2(coordinates, instructions))

  }

  def foldUp(paper: Paper, pos: Int): Paper = {
    val newSize = Coord(paper.size.x, Math.max(pos, paper.size.y - pos - 1))
    val newPoint = paper.points.map(c => Coord(c.x, newSize.y - Math.abs(pos - c.y))).distinct
    Paper(newSize, newPoint)
  }

  def foldLeft(paper: Paper, pos: Int): Paper = {
    val newSize = Coord(Math.max(pos, paper.size.x - pos - 1), paper.size.y)
    val newPoint = paper.points.map(c => Coord(newSize.x - Math.abs(pos - c.x), c.y)).distinct
    Paper(newSize, newPoint)
  }

  def foldDirection(paper: Paper, foldDir: FoldDir): Paper = {
    if (foldDir.dir == "x")
      foldLeft(paper, foldDir.pos)
    else
      foldUp(paper, foldDir.pos)
  }

  def getSize(points: List[Coord]): Coord = {
    val max = points.foldLeft(Coord(0, 0))((s, c) => Coord(Math.max(s.x, c.x), Math.max(s.y, c.y)))
    Coord(max.x + 1, max.y + 1)
  }

  def drawPaper(paper: Paper): String = {
    val sb = Array.fill[Char](paper.size.y, paper.size.x)(' ')
    paper.points.foreach(c => sb(c.y)(c.x) = '■')
    sb.map(_.mkString("", "", "\n")).mkString
  }

  def solvePart1(points: List[Coord], instruction: List[FoldDir]): Int = {
    val size = getSize(points)
    foldDirection(Paper(size, points), instruction.head).points.length
  }


  def solvePart2(points: List[Coord], instruction: List[FoldDir]): String = {
    val size = getSize(points)
    val op = instruction.foldLeft(Paper(size, points))((p, i) => foldDirection(p, i))
    drawPaper(op)
  }
}
