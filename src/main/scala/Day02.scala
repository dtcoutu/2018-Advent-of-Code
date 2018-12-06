object Day02 {
	class Box(val id: String) {
		val idLetterCount: Map[Char,Int] = {
			id.groupBy(_.toChar).map(p => (p._1, p._2.length))
		}

		def hasExactlyTwoMatchingLetters: Boolean = {
			idLetterCount.exists(_._2 == 2)
		}

		def hasExactlyThreeMatchingLetters: Boolean = {
			idLetterCount.exists(_._2 == 3)
		}
	}

	def generateBoxes(input: List[String]): List[Box] = {
		input.map(new Box(_))
	}

	def numberOfBoxesWithTwoMatchingLetters(boxes: List[Box]): Int = {
		boxes.count(_.hasExactlyTwoMatchingLetters)
	}

	def numberOfBoxesWithThreeMatchingLetters(boxes: List[Box]): Int = {
		boxes.count(_.hasExactlyThreeMatchingLetters)
	}

	def generateChecksum(boxes: List[Box]): Int = {
		numberOfBoxesWithTwoMatchingLetters(boxes) * numberOfBoxesWithThreeMatchingLetters(boxes)
	}

	def main(args: Array[String]): Unit = {
		val boxes = generateBoxes(InputReader.readInput("Day02Input.txt"))
		println("Checksum: " + generateChecksum(boxes))
	}
}