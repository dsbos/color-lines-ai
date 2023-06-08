package com.us.dsb.explore.algs.coloredlines.manual.game

import cats.syntax.option._
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{BallColor, Board, BoardOrder, CellAddress, LowerGameState, columnIndices, rowIndices}
import com.us.dsb.explore.algs.coloredlines.manual.game.lines.LineDetector
import com.us.dsb.explore.algs.coloredlines.manual.game.lines.LineDetector.BallArrivalResult

import java.util
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object GameLogicSupport {

  private[game] val InitialBallCount: Int = 5
  private[game] val OnDeckBallCount: Int = 3

  // (was "private[this]" before test calls:)
  private[game] def pickRandomBallColor()(implicit rng: Random): BallColor =
    BallColor.values(rng.nextInt(2 /*???BallKind.values.size*/))

  // (was "private[this]" before test calls:)
  @tailrec
  private[game] def pickRandomEmptyCell(gameState: LowerGameState)(implicit rng: Random): Option[CellAddress] = {
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
  private[game] def placeInitialBalls(gameState: LowerGameState)(implicit rng: Random): BallArrivalResult = {
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

  /** Interpreted meaning/command of a (virtual) tap on a board cell. */
  private[game] sealed trait TapAction

  private[game] object TapAction {

    /** Select tapped-on cell that has ball. */
    private[game] case object SelectBall  extends TapAction

    /** Select tapped-on cell that is empty. */
    private[game] case object SelectEmpty extends TapAction

    /** Cancel selection. */
    private[game] case object Deselect    extends TapAction

    /** (Try to) move ball from selected cell to tapped-on empty cell (if open path). */
    private[game] case object TryMoveBall extends TapAction  //???? should this capture, carry coords?

    /** Pass (move nothing, get more balls placed). */
    private[game] case object Pass        extends TapAction
  }
  import TapAction._

  def interpretTapLocationToTapAction(tapUiState: TapUiGameState,
                                      address: CellAddress): TapAction =
    tapAndStateToTapAction(onABall            = tapUiState.gameState.board.hasABallAt(address),
                           isSelectedAt       = tapUiState.isSelectedAt(address),
                           hasABallSelected   = tapUiState.hasABallSelected,
                           hasAnyCellSelected = tapUiState.hasAnyCellSelected)

  private def tapAndStateToTapAction(onABall: Boolean,
                                     isSelectedAt: Boolean,
                                     hasABallSelected: Boolean,
                                     hasAnyCellSelected: Boolean
                                    ): TapAction = {
    object RenameOrFlattenThis { // grouped/nested re debugger clutter
      sealed trait OnBallOrEmpty
      case object OnBall extends OnBallOrEmpty
      case object OnEmpty extends OnBallOrEmpty

      sealed trait OnSelOrUnsel
      case object OnSel extends OnSelOrUnsel
      case object OnUnsel extends OnSelOrUnsel

      sealed trait HadBallOrNot
      case object HadBall extends HadBallOrNot
      case object NoBall extends HadBallOrNot

      sealed trait HadSelOrNot
      case object HadSel extends HadSelOrNot
      case object NoSel extends HadSelOrNot
    }
    import RenameOrFlattenThis._

    val onBallOrEmpty = if (onABall)            OnBall  else OnEmpty
    val onSelOrUnsel  = if (isSelectedAt)       OnSel   else OnUnsel
    val hadBallOrNot  = if (hasABallSelected)   HadBall else NoBall
    val hadSelOrNot   = if (hasAnyCellSelected) HadSel  else NoSel

    val action: TapAction = {
      val conditions = (onBallOrEmpty, hadBallOrNot, onSelOrUnsel, hadSelOrNot)
      conditions match {
        case (OnBall,  _,       OnUnsel, _     ) => SelectBall  // - tap on ball  when unselected
        case (OnEmpty, HadBall, _,       _     ) => TryMoveBall // - tap on empty when ball selected

        case (OnEmpty, NoBall,  OnUnsel, NoSel ) => SelectEmpty // - tap on empty when no selection
        case (OnEmpty, NoBall,  OnUnsel, HadSel) => Pass        // - tap on empty when selected

        case (_,       _,       OnSel,   _     ) => Deselect    // - tap on either when selected
      }
    }
    action
  }

  private[this] def placeNextBalls(gameState: LowerGameState)(implicit rng: Random): BallArrivalResult = {
    val postPlacementResult =
      //???? for 1 to 3, consume on-deck ball from list, and then place (better for internal state view);;
      // can replenish incrementally or later; later might show up better in internal state view
      gameState.board.getOnDeckBalls
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
                        .withOnDeckBalls(curBoard.getOnDeckBalls.tail)
                        .withBallAt(address, onDeckBall)
                  val postDeueueGameState = curMoveResult.gameState.withBoard(postDequeueBoard)
                  postDeueueGameState
                }
                LineDetector.reapAnyLines(postPlacementGameState, address)
            }
        }

    val replenishedOnDeckBoard = replenishOnDeckBalls(postPlacementResult.gameState.board)
    postPlacementResult.copy(gameState = postPlacementResult.gameState.withBoard(replenishedOnDeckBoard))}

  private[game] def doPass(gameState: LowerGameState)(implicit rng: Random): BallArrivalResult =
    placeNextBalls(gameState)

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

  case class MoveBallResult(gameState: LowerGameState,
                            clearSelection: Boolean)
  {
    println(s"*  $this")
  }

  private[game] def doTryMoveBall(gameState: LowerGameState,
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
        MoveBallResult(gameState, clearSelection = false)
      case true =>
        val moveBallColor = gameState.board.getBallStateAt(from).get  //????
        val postMoveBoard =
          gameState.withBoardWithNoBallAt(from).withBoardWithBallAt(to, moveBallColor)

        val postReapingResult = LineDetector.reapAnyLines(postMoveBoard, to)
        val postPostReadingResult =
          if (! postReapingResult.anyRemovals)
            placeNextBalls(postReapingResult.gameState)
          else
            postReapingResult
        MoveBallResult(postPostReadingResult.gameState, clearSelection = true)
    }
  }

}

