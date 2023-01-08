package com.us.dsb.explore.algs.coloredlines.manual.game

import com.us.dsb.explore.algs.coloredlines.manual.game.board.CellAddress
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{BallKind, Board, BoardOrder, columnIndices, rowIndices}
import com.us.dsb.explore.algs.coloredlines.manual.game.lines.LineDetector

import java.util
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object GameLogicSupport {

  // (was "private[this]" before test calls:)
  private[game] def pickRandomBallKind()(implicit rng: Random): BallKind =
    BallKind.values(rng.nextInt(2 /*???BallKind.values.size*/))

  // (was "private[this]" before test calls:)
  @tailrec
  private[game] def pickRandomEmptyCell(board: Board)(implicit rng: Random): Option[CellAddress] = {
    if (board.isFull)
      None
    else {
      val row = rowIndices(rng.nextInt(BoardOrder))
      val col = columnIndices(rng.nextInt(BoardOrder))
      if (board.getBallStateAt(CellAddress(row, col)).isEmpty)
        Some(CellAddress(row, col))
      else
        pickRandomEmptyCell(board) // loop: try again
    }
  }

  /**
   * @param board
   *   expected to be empty //???? maybe refactor something?
   */
  private[game] def placeInitialBalls(board: Board)(implicit rng: Random): MoveResult = {
    val postPlacementsResult =
      //???? parameterize:
      (1 to 5).foldLeft(MoveResult(board, None)) {
        case (resultSoFar, _) =>
          val address =
            pickRandomEmptyCell(resultSoFar.board).getOrElse(scala.sys.error("Unexpectedly full board"))
          val postPlacementBoard = resultSoFar.board.withBallAt(address, pickRandomBallKind())
          val placementHandlingResult = LineDetector.handleBallArrival(postPlacementBoard, address)
          //???? clean (note: note typical for / flatmap->map case)
          val netAddedScore: Option[Int] =
            (resultSoFar.addedScore.getOrElse(0) +
                placementHandlingResult.addedScore.getOrElse(0)) match {
              case 0 => None
              case n => Some(n)
            }
            resultSoFar.addedScore.flatMap { prevIncr =>
              placementHandlingResult.addedScore.map { newIncr =>
                prevIncr + newIncr
              }
            }
          MoveResult(placementHandlingResult.board, netAddedScore)
      }
    //???? parameterize
    postPlacementsResult.copy(board = postPlacementsResult.board.withOnDeckBalls(List.fill(3)(pickRandomBallKind())))
  }

  private[game] sealed trait Action
  private[game] object Action {
    private[game] case object SelectBall  extends Action
    private[game] case object SelectEmpty extends Action
    private[game] case object Deselect    extends Action
    private[game] case object TryMoveBall extends Action  //???? should this capture, carry coords?
    private[game] case object Pass        extends Action
  }
  import Action._

  def interpretTapLocationToTapAction(board: Board,
                                      address: CellAddress): Action =
    tapAndStateToTapAction(onABall            = board.hasABallAt(address),
                           isSelectedAt       = board.isSelectedAt(address),
                           hasABallSelected   = board.hasABallSelected,
                           hasAnyCellSelected = board.hasAnyCellSelected)

  private def tapAndStateToTapAction(onABall: Boolean,
                                     isSelectedAt: Boolean,
                                     hasABallSelected: Boolean,
                                     hasAnyCellSelected: Boolean
                                    ): Action = {
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

    val action: Action = {
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

  private[this] def placeNextBalls(board: Board)(implicit rng: Random): MoveResult = {
    val postPlacementResult =
      //???? for 1 to 3, consume on-deck ball from list, and then place (better for internal state view);;
      // can replenish incrementally or later; later might show up better in internal state view
      board.getOnDeckBalls
        .foldLeft(MoveResult(board, None)) {
          case (curMoveResult, onDeckBall) =>
            pickRandomEmptyCell(curMoveResult.board) match {
              case None =>  // board full; break out early (game will become over)
                curMoveResult
              case Some(address) =>
                val postDeueueBoard =
                  curMoveResult.board.withOnDeckBalls(curMoveResult.board.getOnDeckBalls.tail)
                val postPlacementBoard = postDeueueBoard.withBallAt(address, onDeckBall)
                LineDetector.handleBallArrival(postPlacementBoard, address)
            }
        }
    //???? parameterize?
    //????? check re duplicate on-deck code (look for other "fill(3)"
    postPlacementResult.copy(board = postPlacementResult.board.withOnDeckBalls(List.fill(3)(pickRandomBallKind())))
  }

  //???? rename?  isn't _user_ move result; is ball move/placement/arrival result
  //?????? possibly change score delta to score; probably change to lower-level game (board balls + score) state,
  // separate from in-progress--vs.--done part of GameState)
  // - maybe use explicit Boolean to indicate whether any lines were harvested
  case class MoveResult(board: Board, addedScore: Option[Int])
  {
    //??? println(s"??? ${this}")
    //??? print("")
  }

  private[game] def doPass(board: Board)(implicit rng: Random): MoveResult =
    placeNextBalls(board)


  //???: likely move core algorithm out; possibly move outer code into Board:
  /**
   * @param toTapCell - must be empty */
  // (was "private[this]" before test calls:)
  private[game] def pathExists(board: Board,
                               fromBallCell: CellAddress,
                               toTapCell: CellAddress): Boolean = {
    //???? CLEAN ALL THIS:
    // - mutability (many eliminate, maybe keep for eventually wanted optimization
    // - looping/recursion and exiting (what is class/method that ~loops until flag return value?_

    // Blocked from (further) "reaching"--by ball or already reached in search.
    val blockedAt: Array[Array[Boolean]] =
      rowIndices.map { row =>
          columnIndices.map { column =>
            board.hasABallAt(CellAddress(row, column))
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


  private[game] def doTryMoveBall(board: Board,  //???? change to game state to carry and update score?
                                  from: CellAddress,
                                  to: CellAddress
                                  )(implicit rng: Random): MoveResult = {
    val canMoveBall = pathExists(board, from, to)
    println("doTryMoveBall.1: canMoveBall " + canMoveBall)
    canMoveBall match {
      case false =>  // can't move--ignore (keep selection state)
        MoveResult(board, None)
      case true =>
        val deselectedBoard = board.withNoSelection
        val moveBallColor = deselectedBoard.getBallStateAt(from).get //????
        val postMoveBoard = deselectedBoard.withNoBallAt(from).withBallAt(to, moveBallColor)

        val postHandlingResult = LineDetector.handleBallArrival(postMoveBoard, to)
        postHandlingResult.addedScore match {
          case None            => placeNextBalls(postHandlingResult.board)
          case Some(increment) => postHandlingResult
        }
    }
  }

}

