import org.scalatest._

class Day02Spec extends FlatSpec with Matchers {
	val sampleInput = """abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab"""

	"Box has two letters" should "return true if box id has exactly two of any letter" in {
		assert(new Day02.Box("abcdef").hasExactlyTwoMatchingLetters == false)
		assert(new Day02.Box("abcccd").hasExactlyTwoMatchingLetters == false)

		assert(new Day02.Box("bababc").hasExactlyTwoMatchingLetters == true)
		assert(new Day02.Box("abbcde").hasExactlyTwoMatchingLetters == true)
		assert(new Day02.Box("aabcdd").hasExactlyTwoMatchingLetters == true)
	}

	"Box has three letters" should "return true if box id has exactly three of any letter" in {
		assert(new Day02.Box("abcdef").hasExactlyThreeMatchingLetters == false)
		assert(new Day02.Box("abbcde").hasExactlyThreeMatchingLetters == false)

		assert(new Day02.Box("abcccd").hasExactlyThreeMatchingLetters == true)
		assert(new Day02.Box("bababc").hasExactlyThreeMatchingLetters == true)
		assert(new Day02.Box("ababab").hasExactlyThreeMatchingLetters == true)
	}

	"Count two letters" should "return the number of ids with two matching letters" in {
		val boxes = Day02.generateBoxes(InputReader.parseInput(sampleInput))

		assert(Day02.numberOfBoxesWithTwoMatchingLetters(boxes) == 4)
	}

	"Count three letters" should "return the number of ids with three matching letters" in {
		val boxes = Day02.generateBoxes(InputReader.parseInput(sampleInput))

		assert(Day02.numberOfBoxesWithThreeMatchingLetters(boxes) == 3)
	}

	"Generate checksum" should "multiple number of ids with two and three matching letters" in {
		val boxes = Day02.generateBoxes(InputReader.parseInput(sampleInput))

		assert(Day02.generateChecksum(boxes) == 12)
	}

	"Box same letters" should "return the letters that are the same between ids" in {
		val box1 = new Day02.Box("abcde")

		assert(box1.sameLetters(new Day02.Box("fghij")) == "")
		assert(box1.sameLetters(new Day02.Box("axcye")) == "ace")
	}

	"Box off by one" should "return true if the ids have only one letter different" in {
		assert(new Day02.Box("fghij").offByOne(new Day02.Box("fguij")) == true)

		assert(new Day02.Box("fghij").offByOne(new Day02.Box("fguiz")) == false)
	}

	"Generate similar ids" should "return box ids that are only off by one" in {
		var input = """abcde
fghij
jfghi
klmno
pqrst
fguij
axcye
wvxyz"""
		
		val expected = new Day02.SimilarBox(
			new Day02.Box("fghij"),
			new Day02.Box("fguij"),
			"fgij")

		val boxes = Day02.generateBoxes(InputReader.parseInput(input))
		assert(Day02.generateSimilarIds(boxes) == List(expected))
	}
}