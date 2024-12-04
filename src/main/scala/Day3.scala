import scala.jdk.StreamConverters._

object Day3 extends App {
  def parse(v: String, acc: Int = 0): Int = {
    if (v.isEmpty) acc
    else
      v.take(4) match {
        case "mul(" => {
          try {
            val Array(a, b) =
              v.drop(4).takeWhile(_ != ')').split(',').map(_.toInt)
            parse(v.dropWhile(_ != ')').drop(1), acc + a * b)
          } catch {
            case _: Throwable => return parse(v.drop(1), acc)
          }
        }
        case _ => {
          parse(v.drop(1), acc)
        }
      }
  }

  def parse_2(v: String, acc: Int = 0, enabled: Boolean = true): Int = {
    if (v.isEmpty) acc
    else
      v.take(4) match {
        case "mul(" if enabled => {
          try {
            val Array(a, b) =
              v.drop(4).takeWhile(_ != ')').split(',').map(_.toInt)
            parse_2(v.dropWhile(_ != ')').drop(1), acc + a * b, enabled)
          } catch {
            case _: Throwable => return parse_2(v.drop(1), acc, enabled)
          }
        }
        case "do()" => {
          parse_2(v.drop(4), acc, true)
        }
        case "don'" if v.startsWith("don't()") => {
          parse_2(v.drop(7), acc, false)
        }
        case _ => {
          parse_2(v.drop(1), acc, enabled)
        }
      }
  }

  val input =
    """xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"""

  val res = parse(input)
  val res2 = parse_2(input)
  println(res)
  println(res2)
}
