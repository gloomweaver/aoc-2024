import scala.jdk.StreamConverters._

object Day1_2 extends App {
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

  val firstCol = parsedValues.map(_(0))
  val secondCol = parsedValues.map(_(1))

  val occurences = secondCol.groupBy(identity).map { case (k, v) =>
    (k, v.length)
  }
  val res = firstCol.map(v => occurences.getOrElse(v, 0) * v).sum

  println(res)
}
