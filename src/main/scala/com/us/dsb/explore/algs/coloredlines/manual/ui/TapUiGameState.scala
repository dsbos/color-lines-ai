// ?????? TODO:  Move out of package "game" (for abstract game) to UI package,
// OR at least move selectionAddress address part out:
package com.us.dsb.explore.algs.coloredlines.manual.ui

import cats.syntax.either._
import com.us.dsb.explore.algs.coloredlines.manual.game.GameLogicSupport
import com.us.dsb.explore.algs.coloredlines.manual.game.board.CellAddress
import com.us.dsb.explore.algs.coloredlines.manual.game.board.LowerGameState
import com.us.dsb.explore.algs.coloredlines.manual.ui.tapapi.{TapAction, TapIntepreter}

import scala.util.Random

// ?????? TODO: Possibly name with "virtual"/"net"/"abstract"/etc.

private[ui] object TapUiGameState {

  // ?????? TODO:  Probably purge. (Not used used.):
  /**
   * Result of completed game.
   */
  private[ui] sealed trait GameResult
  private[ui] object GameResult {
    private[ui] case class Done(score: Int) extends GameResult
  }

  private[this] def makeInitialState(implicit rng: Random): TapUiGameState = {
    val initialPlacementResult = GameLogicSupport.placeInitialBalls(LowerGameState.empty)
    TapUiGameState(initialPlacementResult.gameState, None)
  }

  private[ui] def initial(seed: Long): TapUiGameState = makeInitialState(new Random(seed))
  private[ui] def initial(): TapUiGameState = makeInitialState(new Random())
}
import TapUiGameState._

//???? add random-data state

/** Virtual-tap--UI game state and controller.
 */
private[ui] case class TapUiGameState(gameState: LowerGameState,
                                          selectionAddress: Option[CellAddress]
                                         )(implicit rng: Random) {

  // top-UI selection:

  private[ui] def withCellSelected(address: CellAddress): TapUiGameState =
    copy(selectionAddress = Some(address))

  private[ui] def withNoSelection: TapUiGameState =
    copy(selectionAddress = None)

  private[ui] def hasAnyCellSelected: Boolean =
    selectionAddress.isDefined

  private[ui] def getSelectionCoordinates: Option[CellAddress] =
    selectionAddress

  private[manual] def isSelectedAt(address: CellAddress): Boolean =
    selectionAddress.fold(false)(_ == address)

  private[manual] def hasABallSelected: Boolean =
    selectionAddress.fold(false)(gameState.board.hasABallAt(_))

  // ?? later refine from Either[String, ...] to "fancier" error type
  // ?? maybe add result of move (win/draw/other) with new state (so caller
  //  doesn't have to check state's gameResult; also, think about where I'd add
  //  game history
  // ?????? TODO: Rename; maybe "move" to "... tap move ..."?
  private[ui] def tryMoveAt(tapAddress: CellAddress): Either[String, TapUiGameState] = {
    //???? test
    import TapAction._
    val tapAction = TapIntepreter.interpretTapLocationToTapAction(this, tapAddress)
    println("tryMoveAt: tapAction = " + tapAction)
    val postMoveState: TapUiGameState =
      tapAction match {
        case SelectBall |
             SelectEmpty =>
          withCellSelected(tapAddress)
        case Deselect    =>
          withNoSelection
        case TryMoveBall =>
          //???? should TryMoveBall carry coordinates?:
          //?????? need to split logical moves/plays (e.g., move ball from source
          // to target from top-/selection-level ~UI (keep that separate from cursor-to-taps UI))
          val fromAddress =
            getSelectionCoordinates.getOrElse(sys.error("Shouldn't be able to happen"))

          val tryMoveResult =
            GameLogicSupport.doTryMoveBall(gameState, fromAddress, tapAddress)
          val selectionUpdatedState =
            if (tryMoveResult.clearSelection)
              withNoSelection
            else
              this
          selectionUpdatedState.copy(gameState = tryMoveResult.gameState)
        case Pass        =>
          val passResult = GameLogicSupport.doPass(gameState)
          copy(gameState = passResult.gameState)
              .withNoSelection
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
