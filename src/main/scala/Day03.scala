import javax.print.attribute.standard.MediaSize.Other

object Day03 {
  case class Point(x: Int, y: Int)
  case class Rectangle(width: Int, height: Int)
  case class Claim(id: Int, point: Point, rectangle: Rectangle) {
    val l: Point = point
    val r: Point = Point(point.x + rectangle.width, point.y + rectangle.height)

    def overlaps(other: Claim): Boolean = {
      println("L: " + l + "; R: " + r)
      println("Other L: " + other.l + "; R: " + other.r)
      if (l.x > other.r.x || other.l.x > r.x) false
      else if (l.y < other.r.y || other.l.y < r.y) false
      else true
    }
  }

  def parseClaim(input: String): Claim = {
    val pattern = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r
    val pattern(id, x, y, h, w) = input
    Claim(id.toInt, Point(x.toInt, y.toInt), Rectangle(h.toInt, w.toInt))
  }
}
