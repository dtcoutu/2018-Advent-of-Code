import org.scalatest._

class Day03Spec extends FlatSpec with Matchers {
  "Parse claim" should "convert line of input into a claim" in {
    assert(Day03.parseClaim("#1 @ 1,3: 4x4") ==
      Day03.Claim(1, Day03.Point(1,3), Day03.Rectangle(4,4)))

    assert(Day03.parseClaim("#2 @ 3,1: 4x4") ==
      Day03.Claim(2, Day03.Point(3,1), Day03.Rectangle(4,4)))
  }

  "Claim overlaps" should "indicate if two claims overlap each other" in {
    val baseClaim = Day03.Claim(1, Day03.Point(1,3), Day03.Rectangle(4,4))

    val testClaim1 = Day03.Claim(2, Day03.Point(3,2), Day03.Rectangle(4,4))
    assert(baseClaim.overlaps(testClaim1))

    val testClaim2 = Day03.Claim(3, Day03.Point(5,7), Day03.Rectangle(1,1))
    assert(!baseClaim.overlaps(testClaim2))
  }
}