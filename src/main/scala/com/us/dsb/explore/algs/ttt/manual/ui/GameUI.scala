package com.us.dsb.explore.algs.ttt.manual.ui

import cats.syntax.option._
import cats.syntax.either._
import com.us.dsb.explore.algs.ttt.manual.game.{ColumnIndex, GameState, Index, Player, RowIndex}
import enumeratum.{Enum, EnumEntry}

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

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
  private def getCommand(io: SegregatedTextIO, player: Player): UICommand = {
    val rawCmd = io.readPromptedLine(s"Player $player command?: ")

    import scala.Left
    parseCommand(rawCmd) match {
      case Right(cmd) => cmd
      case Left(msg) =>
        io.printError(msg)
        getCommand(io, player)  // loop
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
  private def markAtSelection(io: SegregatedTextIO,
                              uiState: GameUIState
                             ): GameUIState = {
    val moveResult = uiState.gameState.tryMoveAt(uiState.selectedRow,
                                                 uiState.selectedColumn)
    moveResult match {
      case Right(newGameState) =>
        uiState.copy(gameState = newGameState)
      case Left(errorMsg) =>
        // ??? probably change return value to carry state plus any message
        // (or possibly Either, with caller displaying)
        io.printError(errorMsg)
        uiState  // no change
    }
  }

  private def doQuit: GameUIResult = {
    GameUIResult("Game was quit")
  }

  /**
   *
   * @return next state (`Right`) or disposition of game (`Left`)
   */
  private def doCommand(io: SegregatedTextIO,
                        uiState: GameUIState,
                        command: UICommand
                       ): Either[GameUIResult, GameUIState] = {
    import UICommand._
    command match {
      case Quit =>
        doQuit.asLeft
      case move: UIMoveCommand => // any move-selection command
        moveSelection(uiState, move).asRight
      case Mark =>
        // ?? should following win/draw logic be 1) in markAtSelection (with
        //   other Mark impl. logic), 2) in here, or 3) out in separate method
        //   first calling markAtSelection? (what level is markAtSelection for:
        //   lower-level marking or higher-level moving (marking and maybe
        //   winning/drawing)?) (hmm--similar question re GameState's tryMoveAt) )
        val newState = markAtSelection(io, uiState)
        // Check whether move finished game:
        newState.gameState.gameResult match {
          case None => // game not done yet (after valid _or_ invalid mark try)
            newState.asRight
          case Some(gameResult) =>
            import GameState.GameResult._ // ??? unnest? leave?
            val resultText =
              gameResult match {
                case Draw => "Game ended in draw"
                case Win(player) => s"Player $player won"
              }
            GameUIResult(resultText).asLeft
        }
    }
  }

  // ?? clean looping more (originally was while mess, now recursive; is there
  // better Scala way?)

  /**
   * Logically, loops on prompting for and executing user UI ~commands until
   * game over or quit.
   */
  @tailrec
  private def getAndDoUiCommands(io: SegregatedTextIO,
                                 uiState: GameUIState
                                ): GameUIResult = {
    io.printStateText("")
    io.printStateText(uiState.toDisplayString)
    val command = getCommand(io, uiState.gameState.currentPlayer)

    doCommand(io, uiState, command) match {
      case Right(nextState) =>
        getAndDoUiCommands(io, nextState) // "recurse" to loop
      case Left(uiResult)  =>
        io.printResult(uiResult)
        uiResult
    }
  }

  // ???? soon, probably create class GameUI to hold NameThisIO (to avoid passing
  //   all around); but think about currently pure methods vs. using IO member)

  // ??? add more GameUI tests:
  // - 1:  driving from outside to normal insides--do more: checking SegregatedTextIO output
  // - 2:  driving from outside to special GameState (inject; test double; spy/reporter/?)

  def runGame(io: SegregatedTextIO): GameUIResult = {
    val initialState =
      GameUIState(GameState.initial, RowIndex(Index(1)), ColumnIndex(Index(1)))

    getAndDoUiCommands(io, initialState)

  }

}