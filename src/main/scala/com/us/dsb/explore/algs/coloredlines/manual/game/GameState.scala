package com.us.dsb.explore.algs.coloredlines.manual.game

import cats.syntax.option._
import cats.syntax.either._
import com.us.dsb.explore.algs.coloredlines.manual.game.BallKind
import com.us.dsb.explore.algs.coloredlines.manual.game.GameLogicSupport.MoveResult
import com.us.dsb.explore.algs.coloredlines.manual.game.GameState.GameResult.Done
import com.us.dsb.explore.algs.coloredlines.manual.game.{ColumnIndex, RowIndex}

import scala.util.Random

private[manual] object GameState {

  /**
   * Result of completed game.
   */
  private[manual] sealed trait GameResult //???? change to final score (and maybe stats?)
  private[manual] object GameResult {
    private[manual] case class Done(score: Int) extends GameResult
  }

  private[this] def makeInitialState(rng: Random): GameState = {
    val board = GameLogicSupport.placeInitialBalls(rng, Board.empty)
    GameState(rng, board, 0, None)
  }

  private[game] def initial(seed: Long): GameState = makeInitialState(new Random(seed))
  private[manual] def initial: GameState = makeInitialState(new Random())
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
private[manual] case class GameState(rng: Random,
                                     board: Board,
                                     score: Int, //???? with Positive
                                     gameResult: Option[GameResult]
                                    ) {

  //????? Probably move to GameLogicSupport

  // ?? later refine from Either[String, ...] to "fancier" error type
  // Xx?? maybe add result of move (win/draw/other) with new state (so caller
  //  doesn't have to check state's gameResult; also, think about where I'd add
  //  game history
  private[manual] def tryMoveAt(row: RowIndex,
                                column: ColumnIndex
                               ): Either[String, GameState] = {
    import GameLogicSupport.Action._
    val tapAction = GameLogicSupport.interpretTapLocationToTapAction(board, row, column)
    println("tryMoveAt: tapAction = " + tapAction)
    val moveResult: MoveResult =
      tapAction match {
        case SelectBall |
             SelectEmpty =>
          MoveResult(board.withCellSelected(row, column), None)
        case Deselect    =>
          MoveResult(board.withNoSelection, None)
        case TryMoveBall =>
          //???? should TryMoveBall carry coordinates?:
          val (selRow, selColumn) =
            board.getSelectionCoordinates.getOrElse(sys.error("Shouldn't be able to happen"))
          GameLogicSupport.doTryMoveBall(rng, board, selRow, selColumn, row, column)
        case Pass        =>
          val passResult = GameLogicSupport.doPass(rng, board)
          assert(passResult.addedScore.isEmpty)
          passResult.copy(newBoard = passResult.newBoard.withNoSelection)
      }
    val newScore = score + moveResult.addedScore.getOrElse(0)

    val nextState =
      if (! moveResult.newBoard.isFull) {
        GameState(rng, moveResult.newBoard, newScore, gameResult).asRight
      }
      else {
        GameState(rng, moveResult.newBoard, newScore, Some(Done(newScore))).asRight
      }
    nextState
  }

}
