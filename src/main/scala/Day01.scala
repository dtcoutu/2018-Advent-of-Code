object Day01 {
	def readInput: String = {
		scala.io.Source.fromFile("src/main/resources/Day01Input.txt").mkString
	}

	def parseInput(input: String): List[String] = {
		input.split(scala.util.Properties.lineSeparator).toList
	}

	def convertToNumber(s: String): Long = {
		s.toLong
	}

	def determineResultingFrequency(numbers: List[Long]): Long = {
		numbers.fold(0l) { (acc, i) =>
			acc + i
		}
	}

	def findRepeatFrequency(origFrequencyChanges: List[Long]): Long = {
		def go(frequencyChanges: List[Long], currentFrequency: Long, visitedFrequencies: Set[Long]): Long = {
			frequencyChanges match {
				case Nil => go(origFrequencyChanges, currentFrequency, visitedFrequencies)
				case (frequencyChange :: remainingFrequencyChanges) =>
					val newFrequency = currentFrequency + frequencyChange
					if (visitedFrequencies.contains(newFrequency)) {
						newFrequency
					} else {
						go(remainingFrequencyChanges, newFrequency, visitedFrequencies + newFrequency)
					}
			}
		}

		go(origFrequencyChanges, 0l, Set())
	}

	def processFrequencyChanges: Long = {
		val inputData = InputReader.readInput("Day01Input.txt")
		determineResultingFrequency(inputData.map(convertToNumber))
	}

	def processRepeatFrequency: Long = {
		val inputData = InputReader.readInput("Day01Input.txt")
		findRepeatFrequency(inputData.map(convertToNumber))
	}

	def main(args: Array[String]): Unit = {
		println("Resulting frequency: " + processFrequencyChanges)
		println("First duplicate frequency: " + processRepeatFrequency)
	}
}