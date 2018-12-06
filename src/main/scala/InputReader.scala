object InputReader {
	def readInput(fileName: String): List[String] = {
		parseInput(scala.io.Source.fromFile(s"src/main/resources/${fileName}")
			.mkString)
	}

	def parseInput(input: String): List[String] = {
		input.split(scala.util.Properties.lineSeparator).toList
	}
}