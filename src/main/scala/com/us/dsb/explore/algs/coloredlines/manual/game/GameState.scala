package com.us.dsb.explore.algs.coloredlines.manual.game

import cats.syntax.option._
import cats.syntax.either._
import com.us.dsb.explore.algs.coloredlines.manual.game.board.Board.CellAddress
import com.us.dsb.explore.algs.coloredlines.manual.game.GameLogicSupport.MoveResult
import com.us.dsb.explore.algs.coloredlines.manual.game.GameState.GameResult.Done
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{BallKind, Board}
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{ColumnIndex, RowIndex}

import scala.util.Random

private[manual] object GameState {

  /**
   * Result of completed game.
   */
  private[manual] sealed trait GameResult //???? change to final score (and maybe stats?)
  private[manual] object GameResult {
    private[manual] case class Done(score: Int) extends GameResult
  }

  private[this] def makeInitialState(implicit rng: Random): GameState = {
    val nameThisResult = GameLogicSupport.placeInitialBalls(Board.empty)  //?????? clean
    GameState(nameThisResult.board, nameThisResult.addedScore.getOrElse(0), None)
  }

  private[manual/*game*/] def initial(seed: Long): GameState = makeInitialState(new Random(seed))
  private[manual] def initial(): GameState = makeInitialState(new Random())
}
import GameState._

/**
 *  game state _and_ controller--should functions be separated or together?
 *
 * @param gameResult  `None` means no win or draw yet
 */
 //???? add random-data state

/** Game state AND currently controller.
 */
private[manual] case class GameState(board: Board,
                                     score: Int, //???? with Positive
                                     gameResult: Option[GameResult]
                                    )(implicit rng: Random) {

  //????? Probably move to GameLogicSupport

  // ?? later refine from Either[String, ...] to "fancier" error type
  // Xx?? maybe add result of move (win/draw/other) with new state (so caller
  //  doesn't have to check state's gameResult; also, think about where I'd add
  //  game history
  private[manual] def tryMoveAt(tapAddress: CellAddress): Either[String, GameState] = {
    import GameLogicSupport.Action._
    val tapAction = GameLogicSupport.interpretTapLocationToTapAction(board, tapAddress)
    println("tryMoveAt: tapAction = " + tapAction)
    val moveResult: MoveResult =
      tapAction match {
        case SelectBall |
             SelectEmpty =>
          MoveResult(board.withCellSelected(tapAddress), None)
        case Deselect    =>
          MoveResult(board.withNoSelection, None)
        case TryMoveBall =>
          //???? should TryMoveBall carry coordinates?:
          //???? need to split logical moves/plays (e.g., move ball from source
          // to target from top-/selection-level ~UI (keep that separate from cursor-to-taps UI))
          val fromAddress =
            board.getSelectionCoordinates.getOrElse(sys.error("Shouldn't be able to happen"))
          GameLogicSupport.doTryMoveBall(board, fromAddress, tapAddress)
        case Pass        =>
          val passResult = GameLogicSupport.doPass(board)
          assert(passResult.addedScore.isEmpty)
          passResult.copy(board = passResult.board.withNoSelection)
      }
    val newScore = score + moveResult.addedScore.getOrElse(0)

    val nextState =
      if (! moveResult.board.isFull) {
        GameState(moveResult.board, newScore, gameResult).asRight
      }
      else {
        GameState(moveResult.board, newScore, Some(Done(newScore))).asRight
      }
    nextState
  }

}
