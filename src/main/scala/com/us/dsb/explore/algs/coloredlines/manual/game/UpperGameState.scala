package com.us.dsb.explore.algs.coloredlines.manual.game

import cats.syntax.option._
import cats.syntax.either._
import com.us.dsb.explore.algs.coloredlines.manual.game.board.CellAddress
import com.us.dsb.explore.algs.coloredlines.manual.game.GameLogicSupport.MoveResult
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{BallKind, BoardPlus}
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{ColumnIndex, RowIndex}

import scala.util.Random

private[manual] object UpperGameState {

  /**
   * Result of completed game.
   */
  private[manual] sealed trait GameResult //???? change to final score (and maybe stats?)
  private[manual] object GameResult {
    private[manual] case class Done(score: Int) extends GameResult
  }

  private[this] def makeInitialState(implicit rng: Random): UpperGameState = {
    val initialPlacementResult = GameLogicSupport.placeInitialBalls(BoardPlus.empty)
    //????? probably split GameState level from slightly lower game state
    //  carrying board plus score (probably modifying MoveResult for that)
    UpperGameState(initialPlacementResult.boardPlus, None)
  }

  private[manual/*game*/] def initial(seed: Long): UpperGameState = makeInitialState(new Random(seed))
  private[manual] def initial(): UpperGameState = makeInitialState(new Random())
}
import UpperGameState._

//???? add random-data state

/** Game state AND currently controller.
 * @constructor
 * @param gameResult  `None` means no win or draw yet
 */
private[manual] case class UpperGameState(boardPlus: BoardPlus,
                                          gameResult: Option[GameResult]
                                         )(implicit rng: Random) {

  //????? Probably move to GameLogicSupport

  // ?? later refine from Either[String, ...] to "fancier" error type
  // Xx?? maybe add result of move (win/draw/other) with new state (so caller
  //  doesn't have to check state's gameResult; also, think about where I'd add
  //  game history
  private[manual] def tryMoveAt(tapAddress: CellAddress): Either[String, UpperGameState] = {
    import GameLogicSupport.Action._
    val tapAction = GameLogicSupport.interpretTapLocationToTapAction(boardPlus, tapAddress)
    println("tryMoveAt: tapAction = " + tapAction)
    val moveResult: MoveResult =
      tapAction match {
        case SelectBall |
             SelectEmpty =>
          MoveResult(boardPlus.withCellSelected(tapAddress), false)  //???? ?
        case Deselect    =>
          MoveResult(boardPlus.withNoSelection, false)  //????
        case TryMoveBall =>
          //???? should TryMoveBall carry coordinates?:
          //???? need to split logical moves/plays (e.g., move ball from source
          // to target from top-/selection-level ~UI (keep that separate from cursor-to-taps UI))
          val fromAddress =
            boardPlus.getSelectionCoordinates.getOrElse(sys.error("Shouldn't be able to happen"))
          GameLogicSupport.doTryMoveBall(boardPlus, fromAddress, tapAddress)
          //???? try to move (conditional) .withNoSelection out of doTryMoveBall
          //  up to here; need additional could-move flag (addedScore None would be ambiguous)

        case Pass        =>
          val passResult = GameLogicSupport.doPass(boardPlus)
          passResult.copy(boardPlus = passResult.boardPlus.withNoSelection)
      }

    val nextState =
      if (! moveResult.boardPlus.isFull) {
        UpperGameState(moveResult.boardPlus, gameResult).asRight
      }
      else {
        UpperGameState(moveResult.boardPlus, Some(GameResult.Done(moveResult.boardPlus.getScore))).asRight
      }
    nextState
  }

}
