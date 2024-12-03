import scala.jdk.StreamConverters._

object Day2_1 extends App {
  def isSafe(values: Array[Int]): Boolean = {
    val compare = values.take(2) match {
      case Array(a, b) if a < b => (x: Int, y: Int) => x < y
      case Array(a, b) if a > b => (x: Int, y: Int) => x > y
      case _                    => return false
    }

    values
      .sliding(2)
      .map { case Array(a, b) =>
        val diff = Math.abs(a - b)

        diff >= 1 && diff <= 3 && compare(a, b)
      }
      .forall(identity)
  }

  val input = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""

  val res = input
    .lines()
    .toScala(List)
    .map(_.trim.split("\\s+").map(_.toInt))
    .map(isSafe)
    .filter(identity)
    .length

  println(res)
}
