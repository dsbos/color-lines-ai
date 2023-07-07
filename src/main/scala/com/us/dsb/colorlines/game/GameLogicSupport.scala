package com.us.dsb.colorlines.game

import com.us.dsb.colorlines.game.board.{
  BallColor, Board, BoardReadView, CellAddress, ColumnIndex, RowIndex}
import com.us.dsb.colorlines.game.logic.{FasterPathChecker, LineReaper}

import cats.syntax.option.*

import scala.annotation.tailrec
import scala.util.Random

// ?????? TODO:  Soon return to moving/splitting/cleaning recently-moved GameLogicSupport

// ?????? TODO:  Probably split move-handling methods from support methods.
// ?????? TODO:  Resolve package(s) (game vs. game.logic).
// ?????? TODO:  Maybe start splitting public vs. private packages (interface
//   for clients vs. implementation).  Might split three ways:
//   - essential public interface (e.g., BoardReadView, API to make moves)
//   - private implementation (e.g., mutable Board and mutation code)
//   - non-essential utilities (e.g., path checker, maybe non-mutation part of
//     reaping)

object GameLogicSupport {

  // ???? TODO:  Probably rework random-pick methods so tests can control
  //  sequence of colors and locations, rather than just fixing sequence via seed:

  // (was "private" before test calls:)
  private[game] def pickRandomBallColor()(using rng: Random): BallColor = {
    BallColor.values(rng.nextInt(BallColor.values.size))
  }

  /** Selects an empty cell randomly (if any). */
  // (was "private" before test calls:)
  @tailrec
  private[game] def pickRandomEmptyCell(board: BoardReadView)
                                       (using rng: Random): Option[CellAddress] = {
    if (board.isFull)
      None
    else {
      val row = RowIndex.values(rng.nextInt(RowIndex.values.size))
      val col = ColumnIndex.values(rng.nextInt(ColumnIndex.values.size))
      if (board.getCellStateAt(CellAddress(row, col)).asOption.isEmpty)
        Some(CellAddress(row, col))
      else
        pickRandomEmptyCell(board) // loop: try again
    }
  }

  // ????? TODO:  Maybe clarify code re overwriting even if list not fully
  //  drained (which happens only when board full and game over anyway):
  private def fillOnDeckBalls(board: Board)
                             (using Random): Board =
    board.withOnDeckBalls(List.fill(Parameters.OnDeckBallCount)(pickRandomBallColor()))

  /**
   * Places initial balls on board and on-deck list.
   * @param emptyGameState
   *   expected to be empty
   * @param Random
   *   affects ball colors and placement coordinates
   */
  private[game] def placeInitialBalls(emptyGameState: GameState)
                                     (using Random): GameState = {
    val postPlacementsResult =
      (1 to Parameters.InitialBallCount)
          .foldLeft(emptyGameState) {
            (gameStateSoFar, _) =>
              val address =
                pickRandomEmptyCell(gameStateSoFar.board)
                    .getOrElse(scala.sys.error("Unexpectedly full board"))
              val postPlacementState =
                gameStateSoFar.withBoardWithBallAt(address, pickRandomBallColor())
              val reapResult = LineReaper.reapAnyLines(postPlacementState, address)
              reapResult.gameState
      }
    postPlacementsResult.withBoard(fillOnDeckBalls(postPlacementsResult.board))
  }

  /** Gets initial state (with 5 balls placed). */
  def getInitialState()
                     (using Random): GameState =
    GameLogicSupport.placeInitialBalls(GameState.empty)


  // ???? TODO:  Maybe handle board-full condition more cleanly (don't dequeue
  //   unplaced balls, don't over-replenish).  Maybe fail fast, and don't depend
  //   on (irregular) callers to check whether board becomes full.
  private def placeOndeckBalls(gameState: GameState)
                              (using Random): GameState = {
    val postPlacementResult =
      //???? for 1 to 3, consume on-deck ball from list, and then place (better for internal state view);
      // can replenish incrementally or later; later might show up better in internal state view
      gameState.board.getOndeckBalls
        .foldLeft(gameState) {
          (gameStateSoFar, onDeckBall) =>
            pickRandomEmptyCell(gameStateSoFar.board) match {
              case None          =>  // board full; ignore on-deck ball
                gameStateSoFar
              case Some(address) =>
                val curBoard = gameStateSoFar.board
                val postDequeueBoard = curBoard.withOnDeckBalls(curBoard.getOndeckBalls.tail)
                val postDequeueState = gameStateSoFar.withBoard(postDequeueBoard)
                val postPlacementState = postDequeueState.withBoardWithBallAt(address,
                                                                              onDeckBall)
                val reapResult = LineReaper.reapAnyLines(postPlacementState, address)
                reapResult.gameState
            }
        }
    postPlacementResult.withBoard(fillOnDeckBalls(postPlacementResult.board))
  }

  def doPass(gameState: GameState)
            (using Random): GameState =
    placeOndeckBalls(gameState)

  // ???? TODO:  Maybe rename with "try"/"attempt":
  case class MoveBallResult(gameState: GameState,
                            moveWasValid: Boolean)


  /**
   * Moves ball given validated source ball and target vacant cell locations:
   *
   * @param gameState
   * @param from
   * @param to
   * @param Random
   * @return
   */
  private def doMoveBall(gameState: GameState,
                         from: CellAddress,
                         to: CellAddress)
                        (using Random
                        ): GameState = {
    // ????? TODO:  Rework condition structures here and just above to eliminate this .get:
    val moveBallColor = gameState.board.getCellStateAt(from).asOption.get //??
    val postMoveGameState =
      gameState.withBoardWithNoBallAt(from).withBoardWithBallAt(to, moveBallColor)

    val reapResult = LineReaper.reapAnyLines(postMoveGameState, to)
    val postPostReapingResult =
      if (! reapResult.anyRemovals)
        placeOndeckBalls(reapResult.gameState)
      else
        reapResult.gameState
    postPostReapingResult

  }

  def canMoveBall(board: BoardReadView,
                  from: CellAddress,
                  to: CellAddress): Boolean = {
    // ????? TODO:  Maybe add enumeration of invalid-move conditions:
    if (! board.hasABallAt(from)) {
      false // ?? TODO:  Expand to report no ball there
    }
    else if (board.hasABallAt(to)) {
      false // ?? TODO:  Expand to report no vacancy there
    }
    else if (! FasterPathChecker.pathExists(board, from, to)) {
      false // ?? TODO:  Expand to report no path
    }
    else {
      true
    }
  }

  def doTryMoveBall(gameState: GameState,
                    from: CellAddress,
                    to: CellAddress)
                   (using Random): MoveBallResult = {
    val moveIsValid = canMoveBall(gameState.board, from, to)
    val newGameState =
      if (moveIsValid) {
        doMoveBall(gameState, from, to)
      }
      else {
        gameState
      }
    MoveBallResult(newGameState, moveIsValid)
  }

}

