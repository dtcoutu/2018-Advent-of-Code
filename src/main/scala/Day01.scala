object Day01 {
	def readInput: String = {
		scala.io.Source.fromFile("Day01.txt").mkString
	}

	def parseInput(input: String): List[String] = {
		input.split(scala.util.Properties.lineSeparator).toList
	}

	def convertToNumber(s: String): Long = {
		s.toLong
	}

	def determineFrequency(numbers: List[Long]): Long = {
		numbers.fold(0l) { (acc, i) =>
			acc + i
		}
	}

	def processFrequencyChanges: Long = {
		val inputData = parseInput(readInput)
		determineFrequency(inputData.map(convertToNumber))
	}

	def main(args: Array[String]): Unit = {
		println("Resulting frequency: " + processFrequencyChanges)
	}
}