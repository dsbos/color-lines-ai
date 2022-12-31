package com.us.dsb.explore.algs.coloredlines.manual.ui

import cats.syntax.option._
import cats.syntax.either._
import com.us.dsb.explore.algs.coloredlines.manual.game.{ColumnIndex, XxGameState, Index, RowIndex}
import enumeratum.{Enum, EnumEntry}

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

/** TTT UI controller. */
private[manual] object GameUI {

  // ?? enhance; maybe just put clean strings in; maybe build on GameResult (plus quit case)
  private[ui] case class XxGameUIResult(text: String)


  // ("extends EnumEntry" gets .entryName, enables Enum; "extends Enum[...]"
  // enables (and requires) .values.

  private sealed trait UICommand
  private object UICommand {
    // (Q: Why doesn't UICommand's "sealed" obviate the following one (for
    //   exhaustive-match checks)?
    private[ui] sealed trait UIMoveCommand extends UICommand
    private[ui] case object Up    extends UIMoveCommand
    private[ui] case object Down  extends UIMoveCommand
    private[ui] case object Left  extends UIMoveCommand
    private[ui] case object Right extends UIMoveCommand
    private[ui] case object XxMove  extends UICommand  // tap to select if ..., tap to move if ...
    private[ui] case object Quit  extends UICommand
  }
  // ?? Decide "UICommand._" re little scala.Right ~clashes.

  // (Could put strings in enumerators and use Enum.withName to factor down
  // parse function, but then (sub)layers wouldn't be separated.)

  // ?? revisit Either--use something fancier (MonadError)?
  private def parseCommand(rawCmd: String): Either[String, UICommand] = {
    import UICommand._
    rawCmd match {
      case "u" => Up.asRight
      case "d" => Down.asRight
      case "l" => Left.asRight
      case "r" => Right.asRight
      case "m" => XxMove.asRight
      case "q" => Quit.asRight
      case _ =>
        s"Invalid input \"$rawCmd\"; try u(p), d(own), l(eft), r(right), m(ove), or q(uit)".asLeft
    }
  }

  @tailrec
  private def getCommand(io: SegregatedTextIO): UICommand = {
    val rawCmd = io.readPromptedLine(s"Command?: ")
    parseCommand(rawCmd) match {
      case Right(cmd) => cmd
      case Left(msg) =>
        io.printError(msg)
        getCommand(io)  // loop
    }
  }

  private def moveSelection(uiState: XxGameUIState,
                            moveCommand: UICommand.UIMoveCommand
                           ): XxGameUIState = {
    import UICommand._
    moveCommand match {
      case Up    => uiState.withRowAdjustedBy(-1)
      case Down  => uiState.withRowAdjustedBy(1)
      case Left  => uiState.withColumnAdjustedBy(-1)
      case Right => uiState.withColumnAdjustedBy(1)
    }
  }

  private def moveAtSelection(io: SegregatedTextIO,
                              uiState: XxGameUIState
                             ): XxGameUIState = {
    //???????
    val moveResult = uiState.gameState.xxtryMoveAt(uiState.selectedRow,
                                                 uiState.selectedColumn)
    moveResult match {
      case Right(newGameState) =>
        uiState.copy(gameState = newGameState)
      case Left(errorMsg) =>
        // ??? probably change return value to carry state plus any message
        //   (or possibly Either, with caller displaying)
        io.printError(errorMsg)
        uiState  // no change
    }
  }

  private def doQuit: XxGameUIResult = {
    XxGameUIResult("Game was quit")
  }

  /**
   *
   * @return next state (`Right`) or disposition of game (`Left`)
   */
  private def doCommand(io: SegregatedTextIO,
                        uiState: XxGameUIState,
                        command: UICommand
                       ): Either[XxGameUIResult, XxGameUIState] = {
    import UICommand._
    command match {
      case Quit =>
        doQuit.asLeft
      case move: UIMoveCommand => // any move-selection command
        moveSelection(uiState, move).asRight
      case XxMove =>
        // Xx?? should following win/draw logic be 1) in moveAtSelection (with
        //   other Move impl. logic), 2) in here, or 3) out in separate method
        //   first calling moveAtSelection? (what level is moveAtSelection for:
        //   lower-level marking or higher-level moving (marking and maybe
        //   winning/drawing)?) (hmm--similar question re GameState's tryMoveAt) )
        val newState = moveAtSelection(io, uiState)
        // Check whether move finished game:
        newState.gameState.gameResult match {
          case None => // game not done yet (after valid _or_ invalid mark try)
            newState.asRight
          case Some(gameResult) =>
            import XxGameState.XxGameResult._
            val resultText =
              gameResult match {
                case result: XxxPlaceholderDone => s"Done: $result"
              }
            XxGameUIResult(resultText).asLeft
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
                                 uiState: XxGameUIState
                                ): XxGameUIResult = {
    io.printStateText("")
    io.printStateText(uiState.toDisplayString)
    val command = getCommand(io)

    doCommand(io, uiState, command) match {
      case Right(nextState) =>
        getAndDoUiCommands(io, nextState) // "recurse" to loop
      case Left(uiResult)  =>
        io.printResult(uiResult)
        uiResult
    }
  }

  // ??? soon, probably create class XxGameUI to hold NameThisIO (to avoid passing
  //   all around); but think about currently pure methods vs. using IO member)

  // ??? add more GameUI tests:
  // - 1:  driving from outside to normal insides--do more: checking SegregatedTextIO output
  // - 2:  driving from outside to special GameState (inject; test double; spy/reporter/?)

  def runGame(io: SegregatedTextIO): XxGameUIResult = {
    val initialState =
      XxGameUIState(XxGameState.xxinitial, RowIndex(Index(1)), ColumnIndex(Index(1)))
    //?????? do initial 5 and first 5 on deck by here, maybe in XxGameState.Xxxinitial

    getAndDoUiCommands(io, initialState)
  }

}