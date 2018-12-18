import org.scalatest._

class Day04Spec extends FlatSpec with Matchers {
	"Parse records" should "return records from input string" in {
		assert(
			Day04.parseRecord("[1518-10-18 00:44] falls asleep") ==
				Day04.Record("1518-10-18 00:44", "falls asleep"))
		assert(
			Day04.parseRecord("[1518-04-11 00:46] falls asleep") ==
				Day04.Record("1518-04-11 00:46", "falls asleep"))
	}

	"Sort records" should "return records sorted by time" in {
		val shortInput = """[1518-10-18 00:44] falls asleep
[1518-04-11 00:46] falls asleep"""
		
		val actual = Day04.organizeRecords(InputReader.parseInput(shortInput))

		val expected = List(
			Day04.Record("1518-04-11 00:46", "falls asleep"),
			Day04.Record("1518-10-18 00:44", "falls asleep")
		)

		assert(actual == expected)
	}
}