package u06lab.code


object TicTacToe extends App:

  type Board = Seq[Cell]
  type Game = Seq[Board]
  val bound = 3
  main()

  def main(): Unit =
    val games = computeAnyGame(Player.X, 6)
    println(s"Found ${games.size} games:")
    games.filter(g => isThereAWin(g.last))
        .foreach(printGame)

  def isThereAWin(board: Board): Boolean =
    // Group the cells by rows and check if there is a row with 3 cells of the same player
    val rowWin = board.groupBy(_.x).values.exists(row => row.size == 3 && row.map(_.player).distinct.size == 1)
    // Group the cells by columns and check if there is a column with 3 cells of the same player
    val colWin = board.groupBy(_.y).values.exists(col => col.size == 3 && col.map(_.player).distinct.size == 1)
    // Check if there is a diagonal with 3 cells of the same player
    val diagWin = board.count(c => c.x == c.y) == 3 &&
      board.filter(c => c.x == c.y).map(_.player).distinct.size == 1
    // Check if there is an anti-diagonal with 3 cells of the same player
    val antiDiagWin = board.count(c => c.x + c.y == bound - 1) == 3 &&
      board.filter(c => c.x + c.y == bound - 1).map(_.player).distinct.size == 1
    rowWin || colWin || diagWin || antiDiagWin


  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 1 => playAnyMove(Seq(), player).map(List(_)).to(LazyList)
    case _ =>
        for
          game <- computeAnyGame(player.other, moves - 1)
          win = isThereAWin(game.last)
          board <- if win then LazyList(game.last) else playAnyMove(game.last, player)
        yield if win then game else game :+ board

  private def printGame(game: Game): Unit =
    println("Game:")
    printBoard(game.last)

  private def coordinates: Seq[(Int, Int)] =
    for
      x <- 0 until bound
      y <- 0 until bound
    yield
      (x, y)

  private def printBoard(board: Board): Unit =
    for
      pos <- coordinates
    yield
      board.find(c => c.x == pos._1 && c.y == pos._2) match
        case Some(Cell(_, _, player)) => print(player)
        case None => print("-")
      if pos._2 == bound - 1 then println()

  private def playAnyMove(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 until bound
      y <- 0 until bound
      if !board.exists(c => c.x == x && c.y == y)
    yield
      board :+ Cell(x, y, player)

  case class Cell(x: Int, y: Int, player: Player)

  enum Player:
    case X, O

    def other: Player = this match
      case X => O
      case O => X


