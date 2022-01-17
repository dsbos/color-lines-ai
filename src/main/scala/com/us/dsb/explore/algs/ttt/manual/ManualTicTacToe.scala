package com.us.dsb.explore.algs.ttt.manual

import cats.syntax.option._
import cats.syntax.either._
import enumeratum.EnumEntry

import scala.annotation.tailrec

object ManualTicTacToe extends App {


  sealed trait Command extends EnumEntry

  sealed trait MoveCommand extends Command // ?? why doesn't Command's "sealed" obvious this one?

  object Command {
    case object Up extends MoveCommand
    case object Down extends MoveCommand
    case object Left extends MoveCommand
    case object Right extends MoveCommand
    case object Mark extends Command
    case object Quit extends Command
  }

  sealed trait Player extends EnumEntry

  object Player {
    case object O extends Player
    case object X extends Player
  }

  def parseCommand(rawCmd: String): Either[String, Command] = {
    import Command._
    rawCmd match {
      case "u" => Up.asRight
      case "d" => Down.asRight
      case "l" => Left.asRight
      case "r" => Right.asRight
      case "m" => Mark.asRight
      case "q" => Quit.asRight
      case _ =>
        s"Invalid input \"$rawCmd\"; try u(p), d(own), l(eft), r(right), m(ark), or q(uit)".asLeft
    }
  }

  @tailrec
  def getCommand(player: Player): Command = {
    // ?? clean looking more (was while mess., now recursive; is there better Scala way?

    // ?? clean embedded reference to stdin/console and stdout
    //print(s"Player $player command (u(p), d(own), l(eft), r(right), m(ark), q(uit): ")
    print(s"Player $player command?: ")
    val rawCmd = scala.io.StdIn.readLine()

    parseCommand(rawCmd) match {
      case Right(cmd) => cmd
      case Left(msg) =>
        println(msg)
        getCommand(player)
    }
  }

  object Board {
    /*@newtype deferred*/ private case class Cell(state: Option[Player]) { // if newType, has to be in object
    }

    private object Cell {
      val empty: Cell = Cell(None)
    }

    def initial: Board = new Board(Vector.fill[Cell](3 * 3)(Cell.empty))
  }

  import Board._

  // probably wrap in a GameState with currentPlayer (moved from GameUiState)
  class Board(private val cellArray: Vector[Cell]) {

    import Board._

    private def vectorIndex(row: Index, column: Index): Int = {
      (row - 1) * 3 + (column - 1)
    }

    private def getCellAt(row: Index, column: Index): Cell = {
      cellArray(vectorIndex(row, column))
    }


    private def isEmptyAt(row: Index, column: Index): Boolean = {
      cellArray(vectorIndex(row, column)).state.isEmpty
    }

    def tryMoveAt(player: Player, row: Index, column: Index): Either[String, Board] = {
      if (isEmptyAt(row, column)) {
        val newCellArray = cellArray.updated(vectorIndex(row, column), Cell(player.some))
        val newBoard = new Board(newCellArray)
        newBoard.asRight
      }
      else {
        println("XXXX1:\n" + renderMultiline)
        s"Can't move at row $row, column $column; already marked ${
          getCellAt(row, column)
        }".asLeft // ???? clean getCellAt, etc.
      }
    }

    def renderMultiline: String = {
      (1 to 3).map { row =>
        (1 to 3).map { column =>
          getCellAt(row, column).state match {
            case None => " - "
            case Some(player) => " " + player.toString + " "
          }
        }.mkString("", "|", "")
      }.mkString("\n===========\n")
    }


  }


  // ?? clean (probably refined Int, maybe enum.
  // 1: top row/leftmost column
  type Index = Int

  object Index {
  }

  // ?? somewhere expand to allow for history (maybe via Semigroup or whatever has .compose?)
  case class GameUIState(board         : Board, // ?? expanded to GameState (with currentPlayer)
                         currentPlayer : Player,
                         selectedRow   : Index,
                         selectedColumn: Index) {
    private def wrapToRange(rawIncremented: Index): Index = {
      scala.math.floorMod((rawIncremented - 1), 3) + 1
    }

    def withRowAdustedBy(delta: Int): GameUIState = {
      copy(selectedRow = wrapToRange(selectedRow + delta))
    }

    def withColumnAdustedBy(delta: Int): GameUIState = {
      copy(selectedColumn = wrapToRange(selectedColumn + delta))
    }

    def toDisplayString: String = {
      s"Turn: Player $currentPlayer; marking cursor: <row $selectedRow / column $selectedColumn>"
    }
  }

  case class GameResult(tbd: String)

  object UICommandMethods {
    def moveSelection(state: GameUIState, moveCommand: MoveCommand): GameUIState = {
      import Command._
      moveCommand match {
        case Up => state.withRowAdustedBy(-1)
        case Down => state.withRowAdustedBy(1)
        case Left => state.withColumnAdustedBy(-1)
        case Right => state.withColumnAdustedBy(1)
      }
    }

    def markAtSelection(state: GameUIState): GameUIState = {
      import Player._
      val moveTryResult = state.board.tryMoveAt(state.currentPlayer,
                                                state.selectedRow,
                                                state.selectedColumn)
      moveTryResult match {
        case Right(newBoard) =>
          val newCurrentPlayer = state.currentPlayer match { // ?? move this game(?) logic
            case X => O
            case O => X
          }
          state.copy(board = newBoard, currentPlayer = newCurrentPlayer)
        case Left(errorMsg) =>
          println("TBD: " + errorMsg)
          state
      }


    }

    def doQuit(state: GameUIState): GameResult = {
      GameResult("some result")
    }
  }

  @tailrec
  def getAndDoUiCommands(state: GameUIState): GameResult = {
    println()
    println(state.board.renderMultiline)
    println(state.toDisplayString)


    val command = getCommand(state.currentPlayer)

    import Command._
    import UICommandMethods._
    command match {
      case move: MoveCommand => getAndDoUiCommands(moveSelection(state, move))
      case Mark => getAndDoUiCommands(markAtSelection(state))
      case Quit => doQuit(state)
    }
  }

  val initialState = GameUIState(Board.initial, Player.X, 1, 1)

  val gameResult = getAndDoUiCommands(initialState)
  println("gameResult = " + gameResult)

}
