package com.us.dsb.explore.algs.coloredlines.manual.game

import cats.syntax.either._
import com.us.dsb.explore.algs.coloredlines.manual.game.board.CellAddress
import com.us.dsb.explore.algs.coloredlines.manual.game.board.LowerGameState

import scala.util.Random

private[manual] object UpperGameState {

  /**
   * Result of completed game.
   */
  private[manual] sealed trait GameResult
  private[manual] object GameResult {
    private[manual] case class Done(score: Int) extends GameResult
  }

  private[this] def makeInitialState(implicit rng: Random): UpperGameState = {
    val initialPlacementResult = GameLogicSupport.placeInitialBalls(LowerGameState.empty)
    //????? probably split GameState level from slightly lower game state
    //  carrying board plus score (probably modifying MoveResult for that)
    UpperGameState(initialPlacementResult.gameState, None, None)
  }

  private[manual/*game*/] def initial(seed: Long): UpperGameState = makeInitialState(new Random(seed))
  private[manual] def initial(): UpperGameState = makeInitialState(new Random())
}
import UpperGameState._

//???? add random-data state

/** Game state AND currently controller.
 * @constructor
 * @param gameResult
 *   `None` means not ended yet
 */
private[manual] case class UpperGameState(gameState: LowerGameState,
                                          selectionAddress: Option[CellAddress],
                                          gameResult: Option[GameResult]
                                         )(implicit rng: Random) {

  // top-UI selection:

  private[manual /*game*/ ] def withCellSelected(address: CellAddress): UpperGameState =
    copy(selectionAddress = Some(address))

  private[manual/*game*/] def withNoSelection: UpperGameState =
    copy(selectionAddress = None)

  private[manual/*game*/] def hasAnyCellSelected: Boolean = selectionAddress.isDefined
  private[manual/*game*/] def getSelectionCoordinates: Option[CellAddress] = selectionAddress
  private[manual] def isSelectedAt(address: CellAddress): Boolean =
    selectionAddress.fold(false)(_ == address)

  private[game] def hasABallSelected: Boolean =
    selectionAddress.fold(false)(gameState.hasABallAt(_))

  //????? Probably move to GameLogicSupport

  // ?? later refine from Either[String, ...] to "fancier" error type
  // Xx?? maybe add result of move (win/draw/other) with new state (so caller
  //  doesn't have to check state's gameResult; also, think about where I'd add
  //  game history
  private[manual] def tryMoveAt(tapAddress: CellAddress): Either[String, UpperGameState] = {
    import GameLogicSupport.Action._
    val tapAction = GameLogicSupport.interpretTapLocationToTapAction(this, tapAddress)
    println("tryMoveAt: tapAction = " + tapAction)
    val postMoveState: UpperGameState =
      tapAction match {
        case SelectBall |
             SelectEmpty =>
          withCellSelected(tapAddress)
        case Deselect    =>
          withNoSelection
        case TryMoveBall =>
          //???? should TryMoveBall carry coordinates?:
          //???? need to split logical moves/plays (e.g., move ball from source
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
      if (! postMoveState.gameState.isFull) {
        copy(gameState = postMoveState.gameState, selectionAddress = postMoveState.selectionAddress).asRight
      }
      else {
        UpperGameState(postMoveState.gameState,
                       postMoveState.selectionAddress,
                       Some(GameResult.Done(postMoveState.gameState.getScore))
                       ).asRight
      }
    nextState
  }

}
