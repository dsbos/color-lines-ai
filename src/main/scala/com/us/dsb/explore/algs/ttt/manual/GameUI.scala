package com.us.dsb.explore.algs.ttt.manual

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

  // ?? revisit name
  trait SegregatedTextIO {
    def printStateText(lineOrLines: String): Unit
    def readPromptedLine(prompt: String): String
    def printError(fullLine: String): Unit
    // ?? first, second, or both?:
    def printResult(lineOrLines: String): Unit
    def printResult(result: GameUIResult): Unit = printResult(result.text)
  }

  class BaseConsoleTextIO extends SegregatedTextIO {
    import scala.io.StdIn.readLine

    override def printStateText(lineOrLines: String): Unit = println(lineOrLines)
    override def readPromptedLine(prompt: String): String  = readLine(prompt)
    override def printError(fullLine: String): Unit = println(fullLine)
    override def printResult(lineOrLines: String): Unit = println(lineOrLines)
  }

  object PlainConsoleTextIO extends BaseConsoleTextIO

  object ColoredConsoleTextIO extends BaseConsoleTextIO {
    import scala.io.AnsiColor._
    override def readPromptedLine(prompt: String): String =
      super.readPromptedLine(BLUE + prompt + RESET)
    override def printError(fullLine: String): Unit =
      super.printError(RED + fullLine + RESET)
    override def printResult(lineOrLines: String): Unit =
      super.printResult(BOLD + lineOrLines + RESET)
  }

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
  private def markAtSelection(io: SegregatedTextIO, uiState: GameUIState): GameUIState = {
    val moveResult = uiState.gameState.tryMoveAt(uiState.selectedRow,
                                                 uiState.selectedColumn)
    moveResult match {
      case Right(newGameState) =>
        uiState.copy(gameState = newGameState)
      case Left(errorMsg) =>
        // ?? add to result and have command ~loop show?:
        io.printError(errorMsg)
        uiState  // no change
    }
  }

  private def doQuit(io: SegregatedTextIO, uiState: GameUIState): GameUIResult = {
    GameUIResult("Game was quit").tap {
      io.printResult(_)
    }
  }

  // ?? clean looping more (was while mess, now recursive; is there better Scala way?)
  /**
   * Logically, loops on prompting for and executing user UI ~commands until
   * game over or quit.
   */
  @tailrec
  private def getAndDoUiCommands(io: SegregatedTextIO, uiState: GameUIState): GameUIResult = {
    io.printStateText("")
    io.printStateText(uiState.toDisplayString)

    val command = getCommand(io, uiState.gameState.currentPlayer)

    import UICommand._
    command match {
      // ?? can we factor down the multiple getAndDoUiCommands calls (usefully, in this small case)?
      case Quit =>
        doQuit(io, uiState)
      case move: UIMoveCommand =>  // any move-selection command
        val nextState = moveSelection(uiState, move)
        getAndDoUiCommands(io, nextState)  // loop
      case Mark =>
        val nextState = markAtSelection(io, uiState)
        nextState.gameState.gameResult match {
          case None =>  // game not done yet
            getAndDoUiCommands(io, nextState)  // loop
          case Some(gameResult) =>
            import GameState.GameResult._  // ???
            val textResult =
              gameResult match {
                case Draw        => "Game ended in draw"
                case Win(player) => s"Player $player won"
              }
            // ?? refine from text?
            GameUIResult(textResult).tap {
              io.printResult(_)
            }
        }
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