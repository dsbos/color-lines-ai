package com.us.dsb.explore.algs.coloredlines.manual.game

import cats.syntax.option._
import cats.syntax.either._
import com.us.dsb.explore.algs.coloredlines.manual.game.BallKind
import com.us.dsb.explore.algs.coloredlines.manual.game.GameState.GameResult.PlaceholderDone
import com.us.dsb.explore.algs.coloredlines.manual.game.{ColumnIndex, RowIndex}

import scala.util.Random

private[manual] object GameState {

  /**
   * Result of completed game.
   */
  private[manual] sealed trait GameResult //???? change to final score (and maybe stats?)
  private[manual] object GameResult {
    private[manual] case class PlaceholderDone(stats: Float) extends GameResult
  }

  private[this] def xxselectRandomEmptyCell(rng: Random, board: Board): (RowIndex, ColumnIndex) = {
    val row = rowIndices(rng.nextInt(BoardOrder))
    val col = columnIndices(rng.nextInt(BoardOrder))
    if (board.getBallStateAt(row, col).isEmpty)
      (row, col)
    else
      xxselectRandomEmptyCell(rng, board) // loop: try again
  }

  private def getRandomBallKind(rng: Random): BallKind = BallKind.values(rng.nextInt(BallKind.values.size))

  private[this] def xxmakeInitialState(rng: Random): GameState = {
    val board1 = Board.empty
    val board3 =
      (1 to 5).foldLeft(board1) {
        case (board2, _) =>
          val (row, column) = xxselectRandomEmptyCell(rng, board2)
          board2.withCellHavingBall(row, column, getRandomBallKind(rng))
      }
    //?????? add 3 on-back balls

    GameState(rng, board3, None)
  }

  private[game] def initial(seed: Long): GameState = xxmakeInitialState(new Random(seed))
  private[manual] def initial: GameState = xxmakeInitialState(new Random())
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
                                     gameResult: Option[GameResult]
                                    ) {

  // ?? later refine from Either[String, ...] to "fancier" error type
  // Xx?? maybe add result of move (win/draw/other) with new state (so caller
  //  doesn't have to check state's gameResult; also, think about where I'd add
  //  game history
  private[manual] def tryMoveAt(row: RowIndex,
                                column: ColumnIndex
                               ): Either[String, GameState] = {
    import GameLogicSupport.Action._
    val tapAction = GameLogicSupport.interpretTapLocationToTapAction(board, row, column)
    val nextBoard =
      tapAction match {
        case SelectBall |
             SelectEmpty => board.withCellSelected(row, column)
        case Deselect    => board.withNoSelection
        case TryMoveBall =>
          //?????? do ... path check, ball update, on deck, etc. around here
          println("NIY: " + tapAction); board.withNoSelection
        case Pass        => GameLogicSupport.doPass(rng, board).withNoSelection  //??? clean board ref
      }

    val nextState =
      if (! nextBoard.isFull) {
        GameState(rng, nextBoard, gameResult).asRight
      }
      else {
        GameState(rng, nextBoard, Some(PlaceholderDone(1.23f))).asRight
      }
    nextState
  }

}
