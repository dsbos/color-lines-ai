package com.us.dsb.explore.algs.coloredlines.manual.game

import cats.syntax.option._
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{
  BallColor, Board, BoardOrder, CellAddress, LowerGameState, columnIndices, rowIndices}
import com.us.dsb.explore.algs.coloredlines.manual.game.lines.LineDetector
import com.us.dsb.explore.algs.coloredlines.manual.game.lines.LineDetector.BallArrivalResult
import com.us.dsb.explore.algs.coloredlines.manual.ui.TapUiGameState

import java.util
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object GameLogicSupport {

  private[game] val InitialBallCount: Int = 5
  private[game] val OnDeckBallCount: Int = 3

  // (was "private[this]" before test calls:)
  private[game] def pickRandomBallColor()(implicit rng: Random): BallColor = {
    BallColor.values(rng.nextInt(BallColor.values.size))
  }

  /** Selects an empty cell randomly (if any). */
  // (was "private[this]" before test calls:)
  @tailrec
  private[game] def pickRandomEmptyCell(gameState: LowerGameState)
                                       (implicit rng: Random): Option[CellAddress] = {
    if (gameState.board.isFull)
      None
    else {
      val row = rowIndices(rng.nextInt(BoardOrder))
      val col = columnIndices(rng.nextInt(BoardOrder))
      if (gameState.board.getBallStateAt(CellAddress(row, col)).isEmpty)
        Some(CellAddress(row, col))
      else
        pickRandomEmptyCell(gameState) // loop: try again
    }
  }

  private[this] def replenishOnDeckBalls(board: Board)(implicit rng: Random): Board =
    board.withOnDeckBalls(List.fill(OnDeckBallCount)(pickRandomBallColor()))

  /**
   * @param gameState
   *   expected to be empty //???? maybe refactor something?
   */
  private[manual] def placeInitialBalls(gameState: LowerGameState)
                                       (implicit rng: Random): BallArrivalResult = {
    val postPlacementsResult =
      (1 to InitialBallCount).foldLeft(BallArrivalResult(gameState, anyRemovals = false)) {
        case (resultSoFar, _) =>
          val address =
            pickRandomEmptyCell(resultSoFar.gameState).getOrElse(scala.sys.error("Unexpectedly full board"))
          val postPlacementGameState =
            resultSoFar.gameState.withBoardWithBallAt(address, pickRandomBallColor())
          LineDetector.reapAnyLines(postPlacementGameState, address)
      }

    val replenishedOnDeckBoard = replenishOnDeckBalls(postPlacementsResult.gameState.board)
    postPlacementsResult.copy(gameState = postPlacementsResult.gameState.withBoard(replenishedOnDeckBoard))
  }

  // ???? TODO:  Maybe handle board-full condition more cleanly (don't dequeue
  //   unplaced balls, don't over-replenish).  Maybe fail fast, and don't depend
  //   on (irregular) callers to check whether board becomes full.
  private[this] def placeOndeckBalls(gameState: LowerGameState)
                                    (implicit rng: Random): BallArrivalResult = {
    val postPlacementResult =
      //???? for 1 to 3, consume on-deck ball from list, and then place (better for internal state view);
      // can replenish incrementally or later; later might show up better in internal state view
      gameState.board.getOndeckBalls
        .foldLeft(BallArrivalResult(gameState, anyRemovals = false)) {
          case (curMoveResult, onDeckBall) =>
            pickRandomEmptyCell(curMoveResult.gameState) match {
              case None =>  // board full; break out early (game will become over)
                curMoveResult
              case Some(address) =>
                val postPlacementGameState = {
                  val curBoard = curMoveResult.gameState.board
                  val postDequeueBoard =
                    curBoard
                        .withOnDeckBalls(curBoard.getOndeckBalls.tail)
                        .withBallAt(address, onDeckBall)
                  val postDequeueGameState = curMoveResult.gameState.withBoard(postDequeueBoard)
                  postDequeueGameState
                }
                LineDetector.reapAnyLines(postPlacementGameState, address)
            }
        }

    val replenishedOnDeckBoard = replenishOnDeckBalls(postPlacementResult.gameState.board)
    postPlacementResult.copy(gameState = postPlacementResult.gameState.withBoard(replenishedOnDeckBoard))
  }

  private[manual] def doPass(gameState: LowerGameState)
                            (implicit rng: Random): BallArrivalResult =
    placeOndeckBalls(gameState)

  //???: likely move core algorithm out; possibly move outer code into LowerGameState/Board:
  /**
   * @param toTapCell - must be empty */
  // (was "private[this]" before test calls:)
  private[game] def pathExists(gameState: LowerGameState,
                               fromBallCell: CellAddress,
                               toTapCell: CellAddress): Boolean = {
    //???? CLEAN ALL THIS:
    // - mutability (many eliminate, maybe keep for eventually wanted optimization
    // - looping/recursion and exiting (what is class/method that ~loops until flag return value?_

    // Blocked from (further) "reaching"--by ball or already reached in search.
    val blockedAt: Array[Array[Boolean]] =
      rowIndices.map { row =>
          columnIndices.map { column =>
            gameState.board.hasABallAt(CellAddress(row, column))
          }.toArray
      }.toArray
    val cellsToExpandFrom = mutable.Queue[CellAddress](fromBallCell)

    @tailrec
    def loop: Boolean = {
      cellsToExpandFrom.dequeueFirst(_ => true) match {
        case None =>
          // no more steps/cells to try
          false
        case Some(reachedAddr) =>
          if (reachedAddr == toTapCell) {
            // there is a path
            true
          }
          else {
            // no path yet; queue up neighbors neither blocked nor already processed
            val neighborOffsets = List((+1, 0), (-1, 0), (0, +1), (0, -1))
            neighborOffsets.foreach { case (rowInc, colInc) =>
              val rowOffset: Int = reachedAddr.row.value.value    - 1 + rowInc
              val colOffset: Int = reachedAddr.column.value.value - 1 + colInc
              if (! (0 <= rowOffset && rowOffset < BoardOrder &&
                  0 <= colOffset && colOffset < BoardOrder)) {
              } else if (blockedAt(rowOffset)(colOffset)) {
              }
              else            {
                val neighborAddress = CellAddress(rowIndices(rowOffset),
                                                  columnIndices(colOffset))
                blockedAt(rowOffset).update(colOffset, true)
                cellsToExpandFrom.enqueue(neighborAddress)
              }
            }
            loop
          }
      }
    }

    loop
  }

  // ?????? TODO:  move to (virtual) tap UI class/package:
  // ???? TODO:  Maybe rename with "try"/"attempt":
  case class MoveBallResult(gameState: LowerGameState,
                            moveWasValid: Boolean)
  {
    println(s"*  $this")
  }

  private[manual] def doTryMoveBall(gameState: LowerGameState,
                                    from: CellAddress,
                                    to: CellAddress
                                    )(implicit rng: Random): MoveBallResult = {
    //???? separate move-ball move validation from actually moving (selection
    //   clearing depends on just validity of move, not on deleting any lines)
    //   - see note near some Option/etc. re encoding only valid moves at
    //     that point in move-execution path
    val canMoveBall = pathExists(gameState, from, to)
    canMoveBall match {
      case false =>  // can't move--ignore (keep tap-UI selection state)
        MoveBallResult(gameState, moveWasValid = false)
      case true =>
        val moveBallColor = gameState.board.getBallStateAt(from).get  //????
        val postMoveBoard =
          gameState.withBoardWithNoBallAt(from).withBoardWithBallAt(to, moveBallColor)

        val postReapingResult = LineDetector.reapAnyLines(postMoveBoard, to)
        val postPostReadingResult =
          if (! postReapingResult.anyRemovals)
            placeOndeckBalls(postReapingResult.gameState)
          else
            postReapingResult
        MoveBallResult(postPostReadingResult.gameState, moveWasValid = true)
    }
  }

}

