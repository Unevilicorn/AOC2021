package aoc.day_5_hydrothermal_venture

class Line(val from: Point, val to: Point) {

  def this(x1: Int, y1: Int, x2: Int, y2: Int) = {
    this(new Point(x1, y1), new Point(x2, y2))
  }

  def notDiagonal: Boolean = {
    from.x == to.x || from.y == to.y
  }

  override def toString: String = {
    s"$from -> $to"
  }

  private def betterRangeInc(a: Int, b: Int): Range = {
    Math.min(a, b) to Math.max(a, b)
  }

  def occupiedPoints: List[Point] = {
    val dx = to.x - from.x
    val dy = to.y - from.y

    // Only 1 point
    if (dx == 0 && dy == 0)
      return List(from)
    // horizontal line
    if (dy == 0)
      return betterRangeInc(from.x, to.x).map(x => new Point(x, to.y)).toList

    // vertical line
    if (dx == 0)
      return betterRangeInc(from.y, to.y).map(y => new Point(to.x, y)).toList

    // 45 degrees
    val s = math.signum(dy)
    val sx = Math.min(from.x, to.x)

    if (from.x <= to.x)
      List.range(from.x, to.x + 1).map(x => new Point(x, from.y + s * (x - sx)))
    else
      List.range(to.x, from.x + 1).map(x => new Point(x, to.y - s * (x - sx)))


  }

}
