import java.util.Date

object Day04 {
	case class Record(dateString: String, action: String) {
		def date: Date = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm").parse(dateString)
	}

	def parseRecord(input: String): Record = {
		val pattern = "\\[(.*)\\] (.*)".r
		val pattern(date: String, action: String) = input
		Record(date, action)
	}

	def organizeRecords(input: List[String]): List[Record] = {
		input.map(l => parseRecord(l)).sortBy(_.date.getTime)
	}
}