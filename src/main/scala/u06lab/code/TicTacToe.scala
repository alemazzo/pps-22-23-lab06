package u06lab.code


object TicTacToe extends App:

  type Board = Seq[Cell]
  type Game = Seq[Board]
  val bound = 3
  main()

  def main(): Unit =
    val games = computeAnyGame(Player.X, 2)
    println(s"Found ${games.size} games:")
    games.foreach(printGame)

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 1 => playAnyMove(Seq(), player).map(List(_)).to(LazyList)
    case _ =>
      for
        game <- computeAnyGame(player.other, moves - 1)
        board <- playAnyMove(game.last, player)
      yield
        game :+ board

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


