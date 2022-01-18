package com.us.dsb.explore.algs.ttt.manual

import cats.syntax.option._
import cats.syntax.either._
import enumeratum.EnumEntry

import scala.annotation.tailrec

object ManualTicTacToe extends App {


  sealed trait UICommand extends EnumEntry
  object UICommand {
    // ?? why doesn't UICommand's "sealed" obviate the following one (for exhaustive-match checks?)
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
                         currentPlayer: Player, // ?? move to new GameState
                         selectedRow: RowIndex,
                         selectedColumn: ColumnIndex) {

    // ?? clean up that floorMod; I just want plain mathematical mod
    private def adjustAndwrapToRange(unincremented: Index, delta: Int): Index = {
      // ?? maybe enable auto-wrapping and -unwrapping around match
      val indexOrigin = Index.MinValue.value
      val rangeSize = Index.MaxValue.value - Index.MinValue.value + 1
      val rawIncremented = unincremented.value + delta
      Index.unsafeFrom(
        scala.math.floorMod(rawIncremented - indexOrigin, rangeSize)
            + indexOrigin)
    }

    // ?? should UI work directly with board's index types, or should it
    //   use its own (maybe just to simulate ....)?
    // ?? who should do/provide this index-increment logic? (its just for
    //   our cursor-based row/column specification; what would GUI use, just
    //   9 table-level IDs tied to GUI cells/buttons?);

    def withRowAdustedBy(delta: Int): GameUIState = {
      copy(selectedRow = RowIndex(adjustAndwrapToRange(selectedRow.value, delta)))
    }


    def withColumnAdustedBy(delta: Int): GameUIState = {
      copy(selectedColumn = ColumnIndex(adjustAndwrapToRange(selectedColumn.value, delta)))
    }

    private def renderTableMultilineWithSelection: String = {
      val cellWidth = " X ".length
      val cellSeparator = "|"
      // ?? user new Order or leave using indices declarations?
      val wholeWidth =
        columnIndices.length * cellWidth +
            (columnIndices.length - 1) * cellSeparator.length
      val rowSeparator = "\n" + ("-" * wholeWidth) + "\n"

      rowIndices.map { row =>
        columnIndices.map { column =>
          val cellStateStr =
            board.getMarkAt(row, column) match {
              case None => "-"
              case Some(player) => player.toString
            }
          if (row == selectedRow && column == selectedColumn ) {
            "*" + cellStateStr + "*"
          }
          else {
            " " + cellStateStr + " "
          }
        }.mkString(cellSeparator)
      }.mkString(rowSeparator)

    }

    def toDisplayString: String = {
      renderTableMultilineWithSelection + "\n" +
      s"Turn: Player $currentPlayer; marking cursor: <row $selectedRow / column $selectedColumn>"
    }

  }

  case class GameResult(tbd: String)

  object UICommandMethods {
    import UICommand.UIMoveCommand
    def moveSelection(state: GameUIState,
                      moveCommand: UIMoveCommand): GameUIState = {
      import UICommand._
      moveCommand match {
        case Up    => state.withRowAdustedBy(-1)
        case Down  => state.withRowAdustedBy(1)
        case Left  => state.withColumnAdustedBy(-1)
        case Right => state.withColumnAdustedBy(1)
      }
    }

    // ?? "place mark"?
    def markAtSelection(state: GameUIState): GameUIState = {
      import Player._
      val moveResult = state.board.tryMoveAt(state.currentPlayer,
                                             state.selectedRow,
                                             state.selectedColumn)
      moveResult match {
        case Right(newBoard) =>
          // ?? move this UI-independent game logic:
          val newCurrentPlayer = state.currentPlayer match {
            case X => O
            case O => X
          }
          state.copy(board = newBoard, currentPlayer = newCurrentPlayer)
        case Left(errorMsg) =>
          // ?? clean I/O?
          println(errorMsg)
          state  // no change
      }
    }

    def doQuit(state: GameUIState): GameResult = {
      GameResult("<some result>")
    }
  }

  /**
   * Logically, loops on prompting for and executing user UI ~commands.
   */
  @tailrec
  def getAndDoUiCommands(state: GameUIState): GameResult = {
    println()
    println(state.toDisplayString)

    val command = getCommand(state.currentPlayer)

    import UICommand._
    import UICommandMethods._
    command match {
      // ?? can we factor out the getAndDoUiCommands (usefully, is this small case)?
      case move: UIMoveCommand => getAndDoUiCommands(moveSelection(state, move))
      case Mark                => getAndDoUiCommands(markAtSelection(state))
      case Quit                => doQuit(state)
    }
  }

  val initialState = {
    // ??? clean getting indices; maybe get from index ranges, not contructing here
    GameUIState(Board.initial, Player.X,
                RowIndex(Index(1)), ColumnIndex(Index(1)))
  }


  val gameResult = getAndDoUiCommands(initialState)
  println("gameResult = " + gameResult)

}
