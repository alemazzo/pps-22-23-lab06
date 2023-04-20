package u06lab.code

import java.util.OptionalInt

// Optional!
object ConnectThree extends App:
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   * 0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]
  val bound = 3

  private def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 1 => placeAnyDisk(Seq(), player).map(List(_)).to(LazyList)
    case _ =>
      for
        game <- computeAnyGame(player.other, moves - 1)
        win = isThereAWin(game.last)
        board <- if win then LazyList(game.last) else placeAnyDisk(game.last, player)
      yield if win then game else game :+ board

  private def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  def find(board: Board, x: Int, y: Int): Option[Player] =
    board.find(d => d.x == x && d.y == y).map(_.player)

  import Player.*

  private def firstAvailableRow(board: Board, x: Int): Option[Int] =
    (0 to bound) find (y => !board.exists(d => d.x == x && d.y == y))

  private def isThereAWin(board: Board): Boolean =
    val horizontal = (0 to bound) exists { x =>
      (0 to bound - 2) exists { y =>
        val disks = (0 to 2) map (i => find(board, x, y + i))
        disks.forall(d => d.isDefined && d.get == disks.head.get)
      }
    }
    val vertical = (0 to bound) exists { y =>
      (0 to bound - 2) exists { x =>
        val disks = (0 to 2) map (i => find(board, x + i, y))
        disks.forall(d => d.isDefined && d.get == disks.head.get)
      }
    }
    val diagonal = (0 to bound - 2) exists { x =>
      (0 to bound - 2) exists { y =>
        val disks = (0 to 2) map (i => find(board, x + i, y + i))
        disks.forall(d => d.isDefined && d.get == disks.head.get)
      }
    }
    val antiDiagonal = (0 to bound - 2) exists { x =>
      (2 to bound) exists { y =>
        val disks = (0 to 2) map (i => find(board, x + i, y - i))
        disks.forall(d => d.isDefined && d.get == disks.head.get)
      }
    }
    horizontal || vertical || diagonal || antiDiagonal


  private def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y <- firstAvailableRow(board, x).toSeq
    yield board :+ Disk(x, y, player)

  case class Disk(x: Int, y: Int, player: Player)

  enum Player:
    case X, O

    def other: Player = this match
      case X => O
      case _ => X

  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
  // Exercise 2: implement placeAnyDisk such that..
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O
  println("EX 3: ")
  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 8).filter(_.size == 5).foreach { g =>
      printBoards(g.reverse)
      println()
  }

//  .... .... .... .... ...O
//  .... .... .... ...X ...X
//  .... .... ...O ...O ...O
//  .... ...X ...X ...X ...X
//
//
// .... .... .... .... O...
// .... .... .... X... X...
// .... .... O... O... O...
// .... X... X... X... X...

// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
