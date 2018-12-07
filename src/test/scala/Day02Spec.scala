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
		assert(!new Day02.Box("abcdef").hasExactlyTwoMatchingLetters)
		assert(!new Day02.Box("abcccd").hasExactlyTwoMatchingLetters)

		assert(new Day02.Box("bababc").hasExactlyTwoMatchingLetters)
		assert(new Day02.Box("abbcde").hasExactlyTwoMatchingLetters)
		assert(new Day02.Box("aabcdd").hasExactlyTwoMatchingLetters)
	}

	"Box has three letters" should "return true if box id has exactly three of any letter" in {
		assert(!new Day02.Box("abcdef").hasExactlyThreeMatchingLetters)
		assert(!new Day02.Box("abbcde").hasExactlyThreeMatchingLetters)

		assert(new Day02.Box("abcccd").hasExactlyThreeMatchingLetters)
		assert(new Day02.Box("bababc").hasExactlyThreeMatchingLetters)
		assert(new Day02.Box("ababab").hasExactlyThreeMatchingLetters)
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
		assert(new Day02.Box("fghij").offByOne(new Day02.Box("fguij")))

		assert(!new Day02.Box("fghij").offByOne(new Day02.Box("fguiz")))
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