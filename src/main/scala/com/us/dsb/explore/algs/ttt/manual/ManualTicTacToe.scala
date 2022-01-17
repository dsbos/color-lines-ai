package com.us.dsb.explore.algs.ttt.manual

import cats.syntax.option._
import cats.syntax.either._
import enumeratum.EnumEntry

import scala.annotation.tailrec

object ManualTicTacToe extends App {


  sealed trait UICommand extends EnumEntry
  object UICommand {
    // ?? why doesn't UICommand's "sealed" obviate following one (for exhaustive-match checks?)
    sealed trait UIMoveCommand extends UICommand
    case object Up    extends UIMoveCommand
    case object Down  extends UIMoveCommand
    case object Left  extends UIMoveCommand
    case object Right extends UIMoveCommand
    case object Mark  extends UICommand
    case object Quit  extends UICommand
  }

  def parseCommand(rawCmd: String): Either[String, UICommand] = {
    import UICommand._
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
  def getCommand(player: Player): UICommand = {
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



  // ?? somewhere expand to allow for history (maybe via Semigroup or whatever has .compose?)
  case class GameUIState(board: Board, // ?? expand to GameState (with currentPlayer)
                         currentPlayer: Player,
                         selectedRow: RowIndex,
                         selectedColumn: ColumnIndex) {
    private def wrapToRange(rawIncremented: Int): Int = {
      scala.math.floorMod((rawIncremented - 1), 3) + 1
    }

    def withRowAdustedBy(delta: Int): GameUIState = {
      copy(selectedRow =
             RowIndex.unsafeFrom(wrapToRange(selectedRow.value + delta)))
    }

    def withColumnAdustedBy(delta: Int): GameUIState = {
      copy(selectedColumn =
             ColumnIndex(wrapToRange(selectedColumn.value + delta)))
    }

    def toDisplayString: String = {
      s"Turn: Player $currentPlayer; marking cursor: <row $selectedRow / column $selectedColumn>"
    }
  }

  case class GameResult(tbd: String)

  object UICommandMethods {
    import UICommand.UIMoveCommand
    def moveSelection(state: GameUIState, moveCommand: UIMoveCommand): GameUIState = {
      import UICommand._
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

    import UICommand._
    import UICommandMethods._
    command match {
      case move: UIMoveCommand => getAndDoUiCommands(moveSelection(state, move))
      case Mark => getAndDoUiCommands(markAtSelection(state))
      case Quit => doQuit(state)
    }
  }

  val initialState =
    GameUIState(Board.initial, Player.X, RowIndex(1), ColumnIndex(1))

  val gameResult = getAndDoUiCommands(initialState)
  println("gameResult = " + gameResult)

}
