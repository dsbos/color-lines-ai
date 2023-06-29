package com.us.dsb.colorlines.manual

import com.us.dsb.colorlines.game.{GameLogicSupport, GameState}
import com.us.dsb.colorlines.game.board.CellAddress
import com.us.dsb.colorlines.manual.tapapi.{TapCase, TapInterpreter}

import cats.syntax.either.*
import scala.util.Random

// ????? TODO: Possibly name with "virtual"/"net"/"abstract"/etc.

private[manual] object TapUiGameState {

  // ????? TODO:  Probably purge. (Not used.):
  /**
   * Result of completed game.
   */
  private[manual] sealed trait GameResult
  private[manual] object GameResult {
    private[manual] case class Done(score: Int) extends GameResult
  }

  // ?????? TODO:  Move from UI class/package to ...colorlines.game...:

  private def makeInitialState(using Random): TapUiGameState = {
    val initialPlacementResult = GameLogicSupport.getInitialState()
    TapUiGameState(initialPlacementResult, None)
  }

  private[manual] def initial(seed: Long): TapUiGameState = makeInitialState(using Random(seed))
  private[manual] def initial(): TapUiGameState = makeInitialState(using Random())
}
import TapUiGameState.*

//???? add random-data state

/** Virtual-tap--UI game state and controller.
 */
private[manual] case class TapUiGameState(gameState: GameState,
                                      selectionAddress: Option[CellAddress])
                                     (using Random) {
  // top-UI selection:

  private[manual] def withCellSelected(address: CellAddress): TapUiGameState =
    copy(selectionAddress = Some(address))

  private[manual] def withNoSelection: TapUiGameState =
    copy(selectionAddress = None)

  private[manual] def hasAnyCellSelected: Boolean =
    selectionAddress.isDefined

  private[manual] def getSelectionCoordinates: Option[CellAddress] =
    selectionAddress

  private[manual] def isSelectedAt(address: CellAddress): Boolean =
    selectionAddress.fold(false)(_ == address)

  private[manual] def hasABallSelected: Boolean =
    selectionAddress.fold(false)(gameState.board.hasABallAt(_))

  // ?? later refine from Either[String, ...] to "fancier" error type
  // ?? maybe add result of move (win/draw/other) with new state (so caller
  //  doesn't have to check state's gameResult; also, think about where I'd add
  //  game history
  // ????? TODO: Rename; maybe "move" to "... tap move ..."?
  private[manual] def tryMoveAt(tapAddress: CellAddress): Either[String, TapUiGameState] = {
    //???? test
    import TapCase.*
    val tapCase = TapInterpreter.interpretTapLocationToTapCase(this, tapAddress)
    println("tryMoveAt: tapCase = " + tapCase)
    val postMoveState: TapUiGameState =
      tapCase match {
        case SelectBallTap |
             SelectEmptyTap =>
          withCellSelected(tapAddress)
        case DeselectTap    =>
          withNoSelection
        case TryMoveBallTap =>
          val fromAddress =
            getSelectionCoordinates.getOrElse(sys.error("Shouldn't be able to happen"))

          val tryMoveResult =
            GameLogicSupport.doTryMoveBall(gameState, fromAddress, tapAddress)
          val selectionUpdatedState =
            if (tryMoveResult.moveWasValid)
              withNoSelection
            else
              this
          selectionUpdatedState.copy(gameState = tryMoveResult.gameState)
        case PassTap        =>
          val passResult = GameLogicSupport.doPass(gameState)
          copy(gameState = passResult).withNoSelection
      }

    val nextState =
      if (! postMoveState.gameState.board.isFull) {
        copy(gameState = postMoveState.gameState,
             selectionAddress = postMoveState.selectionAddress).asRight
      }
      else {
        TapUiGameState(postMoveState.gameState,
                       postMoveState.selectionAddress).asRight
      }
    nextState
  }

}
