object InputReader {
	def readInput(fileName: String): String = {
		scala.io.Source.fromFile(s"src/main/resources/${fileName}").mkString
	}
}