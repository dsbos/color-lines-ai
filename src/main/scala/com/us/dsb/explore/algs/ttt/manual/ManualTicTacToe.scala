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
    // ?? clean embedded reference to stdin/console and stdout
    print(s"Player $player command?: ")
    val rawCmd = scala.io.StdIn.readLine()

    parseCommand(rawCmd) match {
      case Right(cmd) => cmd
      case Left(msg) =>
        println(msg)
        getCommand(player)  // loop
    }
  }

  // ??? enhance; maybe just put clean strings in; maybe build on GameResult (plus quit case)
  case class GameUIResult(text: String)


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
          // ?? clean I/O?  add to result and hjave cmd loop show? call ~injected error reporter?
          println(errorMsg)
          uiState  // no change
      }
    }

    def doQuit(uiState: GameUIState): GameUIResult = {
      GameUIResult("Game was quit")
    }
  }

  // ?? clean looping more (was while mess, now recursive; is there better Scala way?)
  /**
   * Logically, loops on prompting for and executing user UI ~commands until
   * game over or quit.
   */
  @tailrec
  def getAndDoUiCommands(uiState: GameUIState): GameUIResult = {
    println()
    println(uiState.toDisplayString)

    val command = getCommand(uiState.gameState.currentPlayer)

    import UICommand._
    import UICommandMethods._
    command match {
      // ?? can we factor down the multiple getAndDoUiCommands calls (usefully, in this small case)?
      case Quit =>
        doQuit(uiState)
      case move: UIMoveCommand =>  // any move-selection command
        getAndDoUiCommands(moveSelection(uiState, move))
      case Mark =>
        val newUiState = markAtSelection(uiState)
        newUiState.gameState.gameResult match {
          case None =>  // game not done yet
            getAndDoUiCommands(newUiState)
          case Some(gameResult) =>

            import GameState.GameResult._
            val textResult =
              gameResult match {
                case Draw        => "Game ended in draw"
                case Win(player) => s"Player $player won"
              }
            GameUIResult(textResult)   // ?? refine from text
        }
    }
  }

  //////////////////////////////////////////////////////////////////////

  val initialState =
    // ?? maybe clean getting indices; maybe get from index ranges, not
    //   constructing here (though here exercises refined type_)
    GameUIState(GameState.initial, RowIndex(Index(1)), ColumnIndex(Index(1)))

  val gameResult: GameUIResult = getAndDoUiCommands(initialState)
  println("Result: " + gameResult.text)

}
