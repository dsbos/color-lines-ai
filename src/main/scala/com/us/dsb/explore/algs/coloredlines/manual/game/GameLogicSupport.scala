package com.us.dsb.explore.algs.coloredlines.manual.game

import com.us.dsb.colorlines.game.board.{
  BallColor, Board, BoardReadView, CellAddress, ColumnIndex, RowIndex}
import com.us.dsb.explore.algs.coloredlines.manual.game.board.LowerGameState
import com.us.dsb.explore.algs.coloredlines.manual.game.lines.LineDetector

import cats.syntax.option.*

import scala.annotation.tailrec
import scala.util.Random

object GameLogicSupport {

  private[game] val InitialBallCount: Int = 5
  private[game] val OnDeckBallCount: Int = 3

  // ???? TODO:  Rework these two so tests can force sequence of colors (and locations):

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

  private def replenishOnDeckBalls(board: Board)(using Random): Board =
    board.withOnDeckBalls(List.fill(OnDeckBallCount)(pickRandomBallColor()))

  /**
   * @param gameState
   *   expected to be empty //???? maybe refactor something?
   */
  private[manual] def placeInitialBalls(gameState: LowerGameState)
                                       (using Random): LowerGameState = {
    val postPlacementsArrivalResult =
      (1 to InitialBallCount)
          .foldLeft(gameState) {
            (gameStateSoFar, _) =>
              val address =
                pickRandomEmptyCell(gameStateSoFar.board)
                    .getOrElse(scala.sys.error("Unexpectedly full board"))
              val postPlacementState =
                gameStateSoFar.withBoardWithBallAt(address, pickRandomBallColor())
              val reapResult = LineDetector.reapAnyLines(postPlacementState, address)
              reapResult.gameState
      }

    val replenishedOnDeckBoard = replenishOnDeckBalls(postPlacementsArrivalResult.board)
    postPlacementsArrivalResult.withBoard(replenishedOnDeckBoard)
  }

  // ???? TODO:  Maybe handle board-full condition more cleanly (don't dequeue
  //   unplaced balls, don't over-replenish).  Maybe fail fast, and don't depend
  //   on (irregular) callers to check whether board becomes full.
  private def placeOndeckBalls(gameState: LowerGameState)
                              (using Random): LowerGameState = {
    val postPlacementResult =
      //???? for 1 to 3, consume on-deck ball from list, and then place (better for internal state view);
      // can replenish incrementally or later; later might show up better in internal state view
      gameState.board.getOndeckBalls
        .foldLeft(gameState) {
          (gameStateSoFar, onDeckBall) =>
            pickRandomEmptyCell(gameStateSoFar.board) match {
              case None          =>  // board full; break out early (game will become over)
                gameStateSoFar
              case Some(address) =>
                val curBoard = gameStateSoFar.board
                val postDequeueBoard = curBoard.withOnDeckBalls(curBoard.getOndeckBalls.tail)
                val postDequeueState = gameStateSoFar.withBoard(postDequeueBoard)
                val postPlacementState = postDequeueState.withBoardWithBallAt(address,
                                                                              onDeckBall)
                val reapResult = LineDetector.reapAnyLines(postPlacementState, address)
                reapResult.gameState
            }
        }

    val replenishedOnDeckBoard = replenishOnDeckBalls(postPlacementResult.board)
    postPlacementResult.withBoard(replenishedOnDeckBoard)
  }

  private[manual] def doPass(gameState: LowerGameState)
                            (using Random): LowerGameState =
    placeOndeckBalls(gameState)

  // ???? TODO:  Maybe rename with "try"/"attempt":
  case class MoveBallResult(gameState: LowerGameState,
                            moveWasValid: Boolean)
  {
    //??println(s"*  $this")
  }

  private[manual] def doTryMoveBall(gameState: LowerGameState,
                                    from: CellAddress,
                                    to: CellAddress
                                    )(using Random): MoveBallResult = {
    //println(s"@@ doTryMoveBall: $from -> $to")
    //???? separate move-ball move validation from actually moving (selection
    //   clearing depends on just validity of move, not on deleting any lines)
    //   - see note near some Option/etc. re encoding only valid moves at
    //     that point in move-execution path

    // ?? TODO:  Maybe add enumeration of invalid-move conditions:
    val moveWasValid =
      if (! gameState.board.hasABallAt(from)) {
        //println(s"@ no ball at from address $from")
        false  // ?? TODO:  Expand to reporting no ball there
      }
      else if (gameState.board.hasABallAt(to)) {
        //println(s"@ no vacancy at to address $from")
        false  // ?? TODO:  Expand to report no vacancy there
      }
      else if (! NameThisGameLogicSupport.pathExists(gameState.board, from, to)) {
        //println(s"@ no path from $from to $to")
        false  // ?? TODO:  Expand to report no path
      }
      else {
        true
      }
    val newGameState =
      moveWasValid match {
        case false =>  // can't move--ignore (keep tap-UI selection state)
          gameState
        case true =>
          // ????? TODO:  Rework condition structures here and just above to eliminate this .get:
          val moveBallColor = gameState.board.getCellStateAt(from).asOption.get //??
          val postMoveGameState =
            gameState.withBoardWithNoBallAt(from).withBoardWithBallAt(to, moveBallColor)

          val reapResult = LineDetector.reapAnyLines(postMoveGameState, to)
          val postPostReapingResult =
            if (! reapResult.anyRemovals)
              placeOndeckBalls(reapResult.gameState)
            else
              reapResult.gameState
          postPostReapingResult
      }
    MoveBallResult(newGameState, moveWasValid)
  }

}

