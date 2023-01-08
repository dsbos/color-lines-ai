package com.us.dsb.explore.algs.coloredlines.manual.game

import com.us.dsb.explore.algs.coloredlines.manual.game.board.CellAddress
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{BallKind, BoardPlus, BoardOrder, columnIndices, rowIndices}
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
  private[game] def pickRandomEmptyCell(boardPlus: BoardPlus)(implicit rng: Random): Option[CellAddress] = {
    if (boardPlus.isFull)
      None
    else {
      val row = rowIndices(rng.nextInt(BoardOrder))
      val col = columnIndices(rng.nextInt(BoardOrder))
      if (boardPlus.getBallStateAt(CellAddress(row, col)).isEmpty)
        Some(CellAddress(row, col))
      else
        pickRandomEmptyCell(boardPlus) // loop: try again
    }
  }

  /**
   * @param board
   *   expected to be empty //???? maybe refactor something?
   */
  private[game] def placeInitialBalls(boardPlus: BoardPlus)(implicit rng: Random): MoveResult = {
    val postPlacementsResult =
      //???? parameterize:
      (1 to 5).foldLeft(MoveResult(boardPlus, false)) {
        case (resultSoFar, _) =>
          val address =
            pickRandomEmptyCell(resultSoFar.boardPlus).getOrElse(scala.sys.error("Unexpectedly full board"))
          val postPlacementBoard = resultSoFar.boardPlus.withBallAt(address, pickRandomBallKind())
          val placementHandlingResult = LineDetector.handleBallArrival(postPlacementBoard, address)
          MoveResult(placementHandlingResult.boardPlus, placementHandlingResult.anyRemovals)
      }
    //???? parameterize
    postPlacementsResult.copy(boardPlus = postPlacementsResult.boardPlus.withOnDeckBalls(List.fill(3)(pickRandomBallKind())))
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

  def interpretTapLocationToTapAction(boardPlus: BoardPlus,
                                      address: CellAddress): Action =
    tapAndStateToTapAction(onABall            = boardPlus.hasABallAt(address),
                           isSelectedAt       = boardPlus.isSelectedAt(address),
                           hasABallSelected   = boardPlus.hasABallSelected,
                           hasAnyCellSelected = boardPlus.hasAnyCellSelected)

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

  private[this] def placeNextBalls(boardPlus: BoardPlus)(implicit rng: Random): MoveResult = {
    val postPlacementResult =
      //???? for 1 to 3, consume on-deck ball from list, and then place (better for internal state view);;
      // can replenish incrementally or later; later might show up better in internal state view
      boardPlus.getOnDeckBalls
        .foldLeft(MoveResult(boardPlus, false)) {
          case (curMoveResult, onDeckBall) =>
            pickRandomEmptyCell(curMoveResult.boardPlus) match {
              case None =>  // board full; break out early (game will become over)
                curMoveResult
              case Some(address) =>
                val postDeueueBoardPlus =
                  curMoveResult.boardPlus.withOnDeckBalls(curMoveResult.boardPlus.getOnDeckBalls.tail)
                val postPlacementBoard = postDeueueBoardPlus.withBallAt(address, onDeckBall)
                LineDetector.handleBallArrival(postPlacementBoard, address)
            }
        }
    //???? parameterize?
    //????? check re duplicate on-deck code (look for other "fill(3)"
    postPlacementResult.copy(boardPlus = postPlacementResult.boardPlus.withOnDeckBalls(List.fill(3)(pickRandomBallKind())))
  }

  //???? rename?  isn't _user_ move result; is ball move/placement/arrival result
  case class MoveResult(boardPlus: BoardPlus,
                        //??? clarify re placing next three balls (re interpreting differently in different contexts
                        anyRemovals: Boolean)
  {
    println(s"??? ${this}")
    //??? print("")
  }

  private[game] def doPass(boardPlus: BoardPlus)(implicit rng: Random): MoveResult =
    placeNextBalls(boardPlus)


  //???: likely move core algorithm out; possibly move outer code into BoardPlus/BoardState:
  /**
   * @param toTapCell - must be empty */
  // (was "private[this]" before test calls:)
  private[game] def pathExists(boardPlus: BoardPlus,
                               fromBallCell: CellAddress,
                               toTapCell: CellAddress): Boolean = {
    //???? CLEAN ALL THIS:
    // - mutability (many eliminate, maybe keep for eventually wanted optimization
    // - looping/recursion and exiting (what is class/method that ~loops until flag return value?_

    // Blocked from (further) "reaching"--by ball or already reached in search.
    val blockedAt: Array[Array[Boolean]] =
      rowIndices.map { row =>
          columnIndices.map { column =>
            boardPlus.hasABallAt(CellAddress(row, column))
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


  private[game] def doTryMoveBall(boardPlus: BoardPlus,  //???? change to game state to carry and update score?
                                  from: CellAddress,
                                  to: CellAddress
                                  )(implicit rng: Random): MoveResult = {
    val canMoveBall = pathExists(boardPlus, from, to)
    canMoveBall match {
      case false =>  // can't move--ignore (keep selection state)
        MoveResult(boardPlus, false)
      case true =>
        val deselectedBoardPlus = boardPlus.withNoSelection
        val moveBallColor = deselectedBoardPlus.getBallStateAt(from).get  //????
        val postMoveBoard = deselectedBoardPlus.withNoBallAt(from).withBallAt(to, moveBallColor)

        val postHandlingResult = LineDetector.handleBallArrival(postMoveBoard, to)
        if (! postHandlingResult.anyRemovals )
          placeNextBalls(postHandlingResult.boardPlus)
        else
          postHandlingResult
    }
  }

}

