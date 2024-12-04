import scala.jdk.StreamConverters._

object Day4 extends App {
  def getDiagonals(input: List[String]): List[String] = {
    val height = matrix.length
    val width = matrix.head.length

    val left = (0 until height + width - 1).map { diagonalIndex =>
      (Math.max(0, diagonalIndex - width + 1) until Math.min(
        height,
        diagonalIndex + 1
      )).map { rowIndex =>
        matrix(rowIndex)(diagonalIndex - rowIndex)
      }.mkString
    }.toList

    val right = (0 until height + width - 1).map { diagonalIndex =>
      (Math.max(0, diagonalIndex - width + 1) until Math.min(
        height,
        diagonalIndex + 1
      )).map { rowIndex =>
        matrix(rowIndex)(width - diagonalIndex - 1 + rowIndex)
      }.mkString
    }.toList
    left ++ right
  }

  def solve1(input: List[String]): Int = {
    val horizontals = matrix
    val verticals = matrix.transpose.map(_.mkString)
    val diagonals = getDiagonals(matrix)

    (horizontals ++ verticals ++ diagonals)
      .map(
        _.sliding(4)
          .filter {
            case "XMAS" => true
            case "SAMX" =>
              true
            case _ => false
          }
          .length
      )
      .sum
  }

  val input =
    """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""

  val matrix = input.lines().toScala(List)

  val res1 = solve1(matrix)
  println(res1)
}
