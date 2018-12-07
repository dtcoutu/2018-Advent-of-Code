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

		def sameLetters(other: Box): String = {
			def go(id1: List[Char], id2: List[Char], matchedLetters: String): String = (id1, id2) match {
				case (h1::t1, h2::t2) => go(t1, t2, matchedLetters + (if (h1 == h2) h1 else ""))
				case _ => matchedLetters
			}

			go(id.toList, other.id.toList, "")
		}

		def offByOne(other: Box): Boolean = {
			assert (id.length == other.id.length)

			sameLetters(other).length == id.length-1
		}
	}

	class SimilarBox(val box1: Box, val box2: Box, val matches: String) {
		override def toString: String = s"box1: ${box1.id}, box2: ${box2.id}, matches: ${matches}"
		override def equals(any: Any): Boolean = any match {
			case x: SimilarBox => box1.id == x.box1.id &&
					box2.id == x.box2.id &&
					matches == x.matches
			case _ => false
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

	def generateSimilarIds(boxes: List[Box]): List[SimilarBox] = {
		def go(remainingBoxes: List[Box], acc: List[SimilarBox]): List[SimilarBox] = remainingBoxes match {
			case Nil => acc
			case box :: Nil => acc
			case box :: tail => {
				val matchingLettersList = tail.filter(t => box.offByOne(t))
					.map(x => new SimilarBox(box, x, box.sameLetters(x)))
				go(tail, acc ++ matchingLettersList)
			}
		}

		go(boxes, List())
	}

	def main(args: Array[String]): Unit = {
		val boxes = generateBoxes(InputReader.readInput("Day02Input.txt"))
		println("Checksum: " + generateChecksum(boxes))
		println("Matching Letters: " + generateSimilarIds(boxes))
	}
}