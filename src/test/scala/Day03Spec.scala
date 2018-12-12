import org.scalatest._

class Day03Spec extends FlatSpec with Matchers {
  "Parse claim" should "convert line of input into a claim" in {
    assert(Day03.parseClaim("#1 @ 1,3: 4x4") ==
      Day03.Claim("1", Day03.Rectangle(1, 5, 3, 7)))

    assert(Day03.parseClaim("#2 @ 3,1: 4x4") ==
      Day03.Claim("2", Day03.Rectangle(3, 7, 1, 5)))
  }

  "Claim grid" should "return the fabric grid for the claim" in {
    val actual1 = Day03.Claim("1", Day03.Rectangle(1,5,3,7)).grid

    val expected1 = List(
      List(".",".",".",".","."),
      List(".",".",".",".","."),
      List(".",".",".",".","."),
      List(".","1","1","1","1"),
      List(".","1","1","1","1"),
      List(".","1","1","1","1"),
      List(".","1","1","1","1")
    )

    assert(actual1 == expected1)

    var actual2 = Day03.Claim("2", Day03.Rectangle(3,7,1,5)).grid
    var expected2 = List(
      List(".",".",".",".",".",".","."),
      List(".",".",".","2","2","2","2"),
      List(".",".",".","2","2","2","2"),
      List(".",".",".","2","2","2","2"),
      List(".",".",".","2","2","2","2")
    )

    assert(actual2 == expected2)
  }

  "Merge row" should "combine rows to a single one" in {
    assert(Day03.mergeRow(List(), List(".",".",".",".","."))
      == List(".",".",".",".","."))
    assert(Day03.mergeRow(List(".","1","X","."), List("2","2","2","."))
      == List("2","X","X","."))
    assert(Day03.mergeRow(List(".","X"), List(".","1","1"))
      == List(".","X","1"))
  }

  "Merge claims" should "combine grids to a single one" in {
    val claim1 = Day03.Claim("1", Day03.Rectangle(1,5,3,7))
    val claim2 = Day03.Claim("2", Day03.Rectangle(3,7,1,5))

    val expected = List(
      List(".",".",".",".",".",".","."),
      List(".",".",".","2","2","2","2"),
      List(".",".",".","2","2","2","2"),
      List(".","1","1","X","X","2","2"),
      List(".","1","1","X","X","2","2"),
      List(".","1","1","1","1"),
      List(".","1","1","1","1")
    )

    val actual = Day03.mergeClaims(List(claim1, claim2))

    assert(actual == expected)
  }

  "Count duplicate claims" should "return the number of squares with multiple claims" in {
    val fabric = List(
      List(".",".",".",".",".",".","."),
      List(".",".",".","2","2","2","2"),
      List(".",".",".","2","2","2","2"),
      List(".","1","1","X","X","2","2"),
      List(".","1","1","X","X","2","2"),
      List(".","1","1","1","1"),
      List(".","1","1","1","1")
    )

    assert(Day03.duplicateClaimCount(fabric) == 4)
  }
}