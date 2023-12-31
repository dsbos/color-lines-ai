// ???? TODO:  Rename to reflect text control (and virtual tap level), probably
//  (eventually) splitting virtual tap level vs. text-control level:
package com.us.dsb.colorlines.manual

import com.us.dsb.colorlines.game.board.{CellAddress, ColumnIndex, Index, RowIndex}

import cats.syntax.either.*

import scala.annotation.tailrec

// ???? TODO:  Rename to reflect text control (and virtual tap level); "TextTapUI"?:
/** Lines game text-controlled virtual-tap UI controller. */
private[manual] object GameUI {

  // ?? enhance; maybe just put clean strings in; maybe build on GameResult (plus quit case)
  private[manual] case class GameUIResult(text: String)


  // ("extends EnumEntry" gets .entryName, enables Enum; "extends Enum[...]"
  // enables (and requires) .values.

  // ???? TODO:  Rename to reflect selection-based tap ~simulation (TextTapUiCommand?):
  private[manual] sealed trait UICommand derives CanEqual
  private[manual] object UICommand {
    // (Q: Why doesn't UICommand's "sealed" obviate the following one (for
    //   exhaustive-match checks)?
    // ????? TODO:  Clarify moving _selection_, not _ball_:
    private[manual] sealed trait UIMoveCommand extends UICommand
    // ????? TODO:  Clean names (e.g., different "move" in UIMoveCommand vs. Move).
    //  maybe GoUp/SelectUp/MoveSelectionUp/SelectUp/PrevRow; Move -> MoveBall?
    //????? test (use (drive with) commands)
    private[manual] case object Up    extends UIMoveCommand
    private[manual] case object Down  extends UIMoveCommand
    private[manual] case object Left  extends UIMoveCommand
    private[manual] case object Right extends UIMoveCommand
    private[manual] case object Move  extends UICommand  // tap to select if ..., tap to move if ...
    private[manual] case object Quit  extends UICommand
  }
  // ?? Decide "UICommand._" re little scala.Right ~clashes.

  // (Could put strings in enumerators and use Enum.withName to factor down
  // parse function, but then (sub)layers wouldn't be separated.)

  // ?? revisit Either--use something fancier (MonadError)?
  private def parseCommand(rawCmdLine: Option[String]): Either[String, UICommand] = {
    import UICommand.*
    rawCmdLine match {
      case None       => Quit.asRight
      case Some(line) =>
        line match {
          case "u" => Up.asRight
          case "d" => Down.asRight
          case "l" => Left.asRight
          case "r" => Right.asRight
          case "m" => Move.asRight
          case "q" => Quit.asRight
          case  _  =>
            s"Invalid input \"$line\"; try u(p), d(own), l(eft), r(right), m(ove), or q(uit)".asLeft
        }
    }
  }

  @tailrec
  private def getCommand(io: SegregatedConsoleIO): UICommand = {
    val cmdLineOpt = io.readPromptedLine(s"Command?: ")
    parseCommand(cmdLineOpt) match {
      case Right(cmd) => cmd
      case Left(msg) =>
        //????? test?
        io.printError(msg)
        getCommand(io)  // loop
    }
  }

  private def moveSelection(uiState: GameUIState,
                            moveCommand: UICommand.UIMoveCommand
                           ): GameUIState = {
    //????? test
    import UICommand.*
    moveCommand match {
      case Up    => uiState.withRowAdjustedBy(-1)
      case Down  => uiState.withRowAdjustedBy(1)
      case Left  => uiState.withColumnAdjustedBy(-1)
      case Right => uiState.withColumnAdjustedBy(1)
    }
  }

  // ????? TODO: maybe "move ball at selection" (vs. "make move at ...")?
  private def moveAtSelection(io: SegregatedConsoleIO,
                              uiState: GameUIState
                             ): GameUIState = {
    //????? test
    val moveResult = uiState.tapUiGameState.tryMoveAt(uiState.cursorAddress)
    moveResult match {
      case Right(newGameState) =>
        uiState.copy(tapUiGameState = newGameState)
      case Left(errorMsg) =>
        // ??? probably change return value to carry state plus any message
        //   (or possibly Either, with caller displaying)
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
  private def doCommand(io: SegregatedConsoleIO,
                        uiState: GameUIState,
                        command: UICommand
                       ): Either[GameUIResult, GameUIState] = {
    import UICommand.*
    command match {
      case Quit =>
        doQuit.asLeft
      case move: UIMoveCommand => // any move-selection command
        moveSelection(uiState, move).asRight
      case Move =>
        //????? test
        // Xx?? should following win/draw logic be 1) in moveAtSelection (with
        //   other Move impl. logic), 2) in here, or 3) out in separate method
        //   first calling moveAtSelection? (what level is moveAtSelection for:
        //   lower-level marking or higher-level moving (marking and maybe
        //   winning/drawing)?) (hmm--similar question re GameState's tryMoveAt) )
        val newState = moveAtSelection(io, uiState)
        // Check whether move finished game:
        if (! newState.tapUiGameState.gameState.board.isFull)
          newState.asRight
        else
          GameUIResult("Score: " + newState.tapUiGameState.gameState.getScore).asLeft
    }
  }

  // ?? clean looping more (originally was while mess, now recursive; is there
  // better Scala way?)

  /**
   * Logically, loops on prompting for and executing user UI ~commands until
   * game over or quit.
   */
  @tailrec
  private def getAndDoUiCommands(io: SegregatedConsoleIO, 
                                 uiState: GameUIState
                                ): GameUIResult = {
    io.printStateText("")
    io.printStateText(uiState.toDisplayString)
    val command = getCommand(io)

    doCommand(io, uiState, command) match {
      case Right(nextState) =>
        //????? test
        getAndDoUiCommands(io, nextState) // "recurse" to loop
      case Left(uiResult)  =>
        io.printResult(uiResult.text)
        uiResult
    }
  }

  // ??? soon, probably create class GameUI to hold NameThisIO (to avoid passing
  //   all around); but think about currently pure methods vs. using IO member)

  // ??? add more GameUI tests:
  // - 1:  driving from outside to normal insides--do more: checking SegregatedConsoleIO output
  // - 2:  driving from outside to special GameState (inject; test double; spy/reporter/?)

  def runGame(io: SegregatedConsoleIO): GameUIResult = {
    val initialState =
      GameUIState(tapUiGameState = TapUiGameState.initial(),
                  cursorAddress  = CellAddress(RowIndex(Index.MinValue),
                                               ColumnIndex(Index.MinValue)))
    getAndDoUiCommands(io, initialState)
  }

}