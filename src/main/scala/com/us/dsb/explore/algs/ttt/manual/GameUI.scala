package com.us.dsb.explore.algs.ttt.manual

import cats.syntax.option._
import cats.syntax.either._
import enumeratum.{Enum, EnumEntry}

import scala.annotation.tailrec

// ?? any substantial benefit to moving internal methods to class? we could
//  avoid some state passing, but only by mutating top-level state member

/** TTT UI controller. */
object GameUI {

  // ??? enhance; maybe just put clean strings in; maybe build on GameResult (plus quit case)
  case class GameUIResult(text: String)

  // ("extends EnumEntry" gets .entryName, enables Enum; "extends Enum[...]"
  // enables (and requires) .values.

  private sealed trait UICommand
  private object UICommand {
    // (Q: Why doesn't UICommand's "sealed" obviate the following one (for
    //   exhaustive-match checks)?
    sealed trait UIMoveCommand extends UICommand
    case object Up    extends UIMoveCommand
    case object Down  extends UIMoveCommand
    case object Left  extends UIMoveCommand
    case object Right extends UIMoveCommand
    case object Mark  extends UICommand
    case object Quit  extends UICommand
  }
  // ?? Decide "UICommand._" re little scala.Right ~clashes.

  // (Could put strings in enumerators and use Enum.withName to factor down
  // parse function, but then layers wouldn't be separated.)

  // ?? revisit String (but may be fine since dealing with input _strings_
  //   from _user_
  // ?? revisit Either--use something fancier (MonadError)?
  private def parseCommand(rawCmd: String): Either[String, UICommand] = {
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
  private def getCommand(player: Player): UICommand = {

    // ?? clean embedded references to stdin/console and stdout
    print(s"Player $player command?: ")
    val rawCmd = scala.io.StdIn.readLine()

    import scala.Left
    parseCommand(rawCmd) match {
      case Right(cmd) => cmd
      case Left(msg) =>
        println(msg)
        getCommand(player)  // loop
    }
  }

  private def moveSelection(uiState: GameUIState,
                            moveCommand: UICommand.UIMoveCommand
                           ): GameUIState = {
    import UICommand._
    moveCommand match {
      case Up    => uiState.withRowAdustedBy(-1)
      case Down  => uiState.withRowAdustedBy(1)
      case Left  => uiState.withColumnAdustedBy(-1)
      case Right => uiState.withColumnAdustedBy(1)
    }
  }

  // ?? "place mark"?
  private def markAtSelection(uiState: GameUIState): GameUIState = {
    val moveResult = uiState.gameState.tryMoveAt(uiState.selectedRow,
                                                 uiState.selectedColumn)
    moveResult match {
      case Right(newGameState) =>
        uiState.copy(gameState = newGameState)
      case Left(errorMsg) =>
        // ?? clean I/O?  add to result and have cmd loop show? call ~injected error reporter?
        println(errorMsg)
        uiState  // no change
    }
  }

  private def doQuit(uiState: GameUIState): GameUIResult = {
    GameUIResult("Game was quit")
  }

  // ?? clean looping more (was while mess, now recursive; is there better Scala way?)
  /**
   * Logically, loops on prompting for and executing user UI ~commands until
   * game over or quit.
   */
  @tailrec
  private def getAndDoUiCommands(uiState: GameUIState): GameUIResult = {
    println()
    println(uiState.toDisplayString)

    val command = getCommand(uiState.gameState.currentPlayer)

    import UICommand._
    command match {
      // ?? can we factor down the multiple getAndDoUiCommands calls (usefully, in this small case)?
      case Quit =>
        doQuit(uiState)
      case move: UIMoveCommand =>  // any move-selection command
        val nextState = moveSelection(uiState, move)
        getAndDoUiCommands(nextState)
      case Mark =>
        val nextState = markAtSelection(uiState)
        nextState.gameState.gameResult match {
          case None =>  // game not done yet
            getAndDoUiCommands(nextState)
          case Some(gameResult) =>
            import GameState.GameResult._  // ???
            val textResult =
              gameResult match {
                case Draw        => "Game ended in draw"
                case Win(player) => s"Player $player won"
              }
            GameUIResult(textResult)   // ?? refine from text?
        }
    }
  }

  def runGame(): GameUIResult = {
    val initialState =
      GameUIState(GameState.initial, RowIndex(Index(1)), ColumnIndex(Index(1)))

    getAndDoUiCommands(initialState)

  }

}