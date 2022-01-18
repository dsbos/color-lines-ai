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

  case class GameUIResult(tbd: String)

  object UICommandMethods {

    import UICommand.UIMoveCommand
    def moveSelection(uiState: GameUIState,
                      moveCommand: UIMoveCommand): GameUIState = {
      import UICommand._
      moveCommand match {
        case Up    => uiState.withRowAdustedBy(-1)
        case Down  => uiState.withRowAdustedBy(1)
        case Left  => uiState.withColumnAdustedBy(-1)
        case Right => uiState.withColumnAdustedBy(1)
      }
    }

    // ?? "place mark"?
    def markAtSelection(uiState: GameUIState): GameUIState = {
      val moveResult = uiState.gameState.tryMoveAt(uiState.selectedRow,
                                                   uiState.selectedColumn)
      moveResult match {
        case Right(newGameState) =>
          uiState.copy(gameState = newGameState)
        case Left(errorMsg) =>
          // ?? clean I/O?
          println(errorMsg)
          uiState  // no change
      }
    }

    def doQuit(uiState: GameUIState): GameUIResult = {
      GameUIResult("<some result>")
    }
  }

  /**
   * Logically, loops on prompting for and executing user UI ~commands.
   */
  @tailrec
  def getAndDoUiCommands(uiState: GameUIState): GameUIResult = {
    println()
    println(uiState.toDisplayString)

    val command = getCommand(uiState.gameState.currentPlayer)

    import UICommand._
    import UICommandMethods._
    command match {
      // ?? can we factor out the getAndDoUiCommands (usefully, in this small case)?
      case Quit =>
        doQuit(uiState)
      case move: UIMoveCommand =>
        getAndDoUiCommands(moveSelection(uiState, move))

      case Mark =>
        val newUiState = markAtSelection(uiState)
        newUiState.gameState.gameResult match {
          case None => getAndDoUiCommands(newUiState)
          case Some(gameResult) =>
            GameUIResult("Game finished ... " + gameResult)  // ??? "UI-ifiy"; do .toString higher up


        }
    }
  }

  //////////////////////////////////////////////////////////////////////

  val initialState = {
    // ?? maybe clean getting indices; maybe get from index ranges, not
    //   constructing here (though here exercises refined type_)
    GameUIState(GameState.initial, RowIndex(Index(1)), ColumnIndex(Index(1)))
  }


  val gameResult: GameUIResult = getAndDoUiCommands(initialState)
  println("gameResult = " + gameResult)

}
