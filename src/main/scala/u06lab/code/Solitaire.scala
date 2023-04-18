package u06lab.code

import scala.::

object Domain:
  case class Position(x: Int, y: Int)

  case class BoardSize(width: Int, height: Int):
    def center: Position = Position(width / 2, height / 2)
    def total: Int = width * height
    def widthRange: Range = 0 until width
    def heightRange: Range = 0 until height

  case class Board(size: BoardSize, positions: Seq[Position])

  case class Solution(positions: Seq[Position]):
    export positions.{last, contains, indexOf}
    def :+(position: Position): Solution = Solution(positions :+ position)

  object Solution:
    def center(size: BoardSize): Solution = Solution(size.center)

    def apply(position: Position, positions: Position*): Solution = new Solution(position +: positions)
    def apply(positions: Seq[Position]): Solution = new Solution(positions)

object Solitaire extends App:

  import Domain.*

  @main def main() =
    val boardSize = BoardSize(5, 5)
    val solutions = placeMarks(boardSize, boardSize.total - 12)
    solutions.foreach(p => println(render(p, boardSize.width, boardSize.height) + "\n"))
    println("Solutions: " + solutions.size)

  private def placeMarks(size: BoardSize, n: Int): Seq[Solution] = n match
    case 1 => Seq(Solution.center(size))
    case _ =>
      for
        solution <- placeMarks(size, n - 1)
        x <- size.widthRange
        y <- size.heightRange
        spot = Position(x, y)
        if !solution.contains(spot)
        if isLegalMove(solution, spot)
      yield solution :+ spot

  // Max distance horizontally and vertically by 2 or diagonally 1
  private def isLegalMove(solution: Solution, position: Position) =
    ((solution.last.x - position.x).abs + (solution.last.y - position.y).abs) == 2

  private def render(solution: Solution, width: Int, height: Int): String =
    val reversed = solution //.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
                    number = reversed.indexOf(Position(x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")