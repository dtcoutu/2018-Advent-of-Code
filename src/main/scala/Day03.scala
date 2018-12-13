import javax.print.attribute.standard.MediaSize.Other

object Day03 {
  case class Point(x: Int, y: Int)
  case class Rectangle(left: Int, right: Int, top: Int, bottom: Int)
  case class Claim(id: String, rectangle: Rectangle) {
    def grid: List[List[String]] = {
      (0 until rectangle.bottom).toList.map { i =>
        if (i < rectangle.top) List.fill(rectangle.right)(".")
        else List.fill(rectangle.left)(".") ++ List.fill(rectangle.right-rectangle.left)(id)
      }
    }
    def overlaps(other: Claim): Boolean = {
      if (rectangle.left < other.rectangle.right
        && rectangle.right > other.rectangle.left
        && rectangle.top < other.rectangle.bottom
        && rectangle.bottom > other.rectangle.top)
        true
      else false
    }
  }

  def mergeClaims(claims: List[Claim]): List[List[String]] = {
    def go(remainingClaims: List[Claim], fabricGrid: List[List[String]]): List[List[String]] =
      remainingClaims match {
        case claim :: tail => go(tail, mergeClaim(fabricGrid, claim))
        case Nil => fabricGrid
      }

    go(claims, List())
  }

  def mergeClaim(fabricGrid: List[List[String]], claim: Claim): List[List[String]] = {
    (fabricGrid.zipAll(claim.grid, List(), List())).map{ mergeRow(_) }
  }

  def mergeRow(row: (List[String], List[String])): List[String] = {
    (row._1.zipAll(row._2, ".", ".")).map{ mergeCell(_) }
  }

  def mergeCell(cell: (String, String)): String = cell match {
    case (".", ".") => "."
    case (_, "X") => "X"
    case ("X", _) => "X"
    case (".", id) => id
    case (id, ".") => id
    case (_, _) => "X"
  }

  def duplicateClaimCount(fabricGrid: List[List[String]]): Int = {
    fabricGrid.map { row =>
      row.count(_ == "X")
    }.foldLeft(0)(_ + _)
  }

  def generateClaims(input: List[String]): List[Claim] = {
    input.map(parseClaim)
  }

  def parseClaim(input: String): Claim = {
    val pattern = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r
    val pattern(id, l, t, w, h) = input
    Claim(id,
      Rectangle(l.toInt, (l.toInt + w.toInt),
                t.toInt, (t.toInt + h.toInt)))
  }

  def findNonOverlappedClaim(claims: List[Claim]): Claim = {
    def go(remainingClaims: List[Claim]): Claim = remainingClaims match {
      case claim :: tail => {
        val otherClaims = claims.filter(_.id != claim.id)
        if (otherClaims.exists(_.overlaps(claim))) go(tail)
        else claim
      }
    }

    go(claims)
  }

  def main(args: Array[String]): Unit = {
    val claims = generateClaims(InputReader.readInput("Day03Input.txt"))
    println("Overlapping Claims: " + duplicateClaimCount(mergeClaims(claims)))
    println("Non-Overlapping Claim: " + findNonOverlappedClaim(claims))
  }
}
