import scala.jdk.StreamConverters._

object Day1_1 extends App {
  val input = """3   4
4   3
2   5
1   3
3   9
3   3"""

  val parsedValues = input
    .lines()
    .toScala(List)
    .map(_.trim.split("\\s+").map(_.toInt))

  val firstCol = parsedValues.map(_(0)).sorted
  val secondCol = parsedValues.map(_(1)).sorted

  val diff = firstCol
    .zip(secondCol)
    .map { case (a, b) =>
      Math.abs(a - b)
    }
    .sum
  println(diff)
}
