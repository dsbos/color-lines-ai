package com.us.dsb.explore.algs.coloredlines.manual.game

import com.us.dsb.colorlines.game.board.{
  BallColor, BoardOrder, CellAddress, IndexOrigin, ColumnIndex, RowIndex}
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{Board, LowerGameState}
import com.us.dsb.explore.algs.coloredlines.manual.game.lines.LineDetector
import com.us.dsb.explore.algs.coloredlines.manual.game.lines.LineDetector.BallArrivalResult

import cats.syntax.option.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object GameLogicSupport {

  private[game] val InitialBallCount: Int = 5
  private[game] val OnDeckBallCount: Int = 3

  // (was "private" before test calls:)
  private[game] def pickRandomBallColor()(using rng: Random): BallColor = {
    BallColor.values(rng.nextInt(BallColor.values.size))
  }

  /** Selects an empty cell randomly (if any). */
  // (was "private" before test calls:)
  @tailrec
  private[game] def pickRandomEmptyCell(gameState: LowerGameState)
                                       (using rng: Random): Option[CellAddress] = {
    if (gameState.board.isFull)
      None
    else {
      val row = RowIndex.values(rng.nextInt(RowIndex.values.size))
      val col = ColumnIndex.values(rng.nextInt(ColumnIndex.values.size))
      if (gameState.board.getBallStateAt(CellAddress(row, col)).isEmpty)
        Some(CellAddress(row, col))
      else
        pickRandomEmptyCell(gameState) // loop: try again
    }
  }

  private def replenishOnDeckBalls(board: Board)(using Random): Board =
    board.withOnDeckBalls(List.fill(OnDeckBallCount)(pickRandomBallColor()))

  /**
   * @param gameState
   *   expected to be empty //???? maybe refactor something?
   */
  private[manual] def placeInitialBalls(gameState: LowerGameState)
                                       (using Random): BallArrivalResult = {
    val postPlacementsResult =
      (1 to InitialBallCount)
          .foldLeft(BallArrivalResult(gameState, anyRemovals = false)) {
            (resultSoFar, _) =>
              val address =
                pickRandomEmptyCell(resultSoFar.gameState)
                    .getOrElse(scala.sys.error("Unexpectedly full board"))
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
  private def placeOndeckBalls(gameState: LowerGameState)
                              (using Random): BallArrivalResult = {
    val postPlacementResult =
      //???? for 1 to 3, consume on-deck ball from list, and then place (better for internal state view);
      // can replenish incrementally or later; later might show up better in internal state view
      gameState.board.getOndeckBalls
        .foldLeft(BallArrivalResult(gameState, anyRemovals = false)) {
          (curMoveResult, onDeckBall) =>
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
                            (using Random): BallArrivalResult =
    placeOndeckBalls(gameState)

  //???: likely move core algorithm out; possibly move outer code into LowerGameState/Board:
  /**
   * @param toTapCell - must be empty */
  // (was "private" before test calls:)
  private[game] def pathExists(gameState: LowerGameState,
                               fromBallCell: CellAddress,
                               toTapCell: CellAddress): Boolean = {
    //??println(s"pathExists: fromBallCell = $fromBallCell, toTapCell = $toTapCell")
    //???? CLEAN ALL THIS:
    // - mutability (many eliminate, maybe keep for eventually wanted optimization)
    // - looping/recursion and exiting (what is class/method that ~loops until flag return value?_

    // Blocked from (further) "reaching"--by ball or already reached in search.
    val blockedAt: Array[Array[Boolean]] =
      RowIndex.values.map { row =>
          ColumnIndex.values.map { column =>
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
            neighborOffsets.foreach { (rowInc, colInc) =>
              val rowOffset: Int = reachedAddr.row.raw.value    - IndexOrigin + rowInc
              val colOffset: Int = reachedAddr.column.raw.value - IndexOrigin + colInc
              // ???? TODO:  Use Index.MinValue/.MaxValue, etc.?
              if (! (   0 <= rowOffset && rowOffset < BoardOrder
                     && 0 <= colOffset && colOffset < BoardOrder)) {
                // off board
              } else if (blockedAt(rowOffset)(colOffset)) {
                // blocked or traversed
              }
              else {
                // open; note traversed and try neighboring cells:
                blockedAt(rowOffset).update(colOffset, true)
                val neighborAddress = CellAddress(RowIndex.values(rowOffset),
                                                  ColumnIndex.values(colOffset))
                cellsToExpandFrom.enqueue(neighborAddress)
              }
            }
            loop
          }
      }
    }

    loop
  }

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
      else if (! pathExists(gameState, from, to)) {
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
          val moveBallColor = gameState.board.getBallStateAt(from).get  //????
          val postMoveBoard =
            gameState.withBoardWithNoBallAt(from).withBoardWithBallAt(to, moveBallColor)

          val postReapingResult = LineDetector.reapAnyLines(postMoveBoard, to)
          val postPostReadingResult =
            if (! postReapingResult.anyRemovals)
              placeOndeckBalls(postReapingResult.gameState)
            else
              postReapingResult
          postPostReadingResult.gameState
      }
    MoveBallResult(newGameState, moveWasValid)
  }

}

