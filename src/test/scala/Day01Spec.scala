import org.scalatest._

class Day01Spec extends FlatSpec with Matchers {
	val sampleInput = """+1
-2
+3
+1"""

	"Parse input" should "return a list of number strings" in {
		assert(Day01.parseInput(sampleInput).size == 4)
	}

	"Convert to number" should "change string to a number" in {
		assert(Day01.convertToNumber("+3") == 3)
		assert(Day01.convertToNumber("-10") == -10)
	}

	"Determine frequency" should "sum numbers to get the frequency change" in {
		assert(Day01.determineFrequency(List(1,-2,3,1)) == 3)
	}
}