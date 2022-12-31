package com.us.dsb.explore.algs.coloredlines.manual.game

import scala.annotation.tailrec
import scala.util.Random

object GameLogicSupport {

  private[this] def pickRandomBallKind(rng: Random): BallKind = BallKind.values(rng.nextInt(BallKind.values.size))

  @tailrec
  private[this] def pickRandomEmptyCell(rng: Random, board: Board): Option[(RowIndex, ColumnIndex)] = {
    if (board.isFull)
      None
    else {
      val row = rowIndices(rng.nextInt(BoardOrder))
      val col = columnIndices(rng.nextInt(BoardOrder))
      if (board.getBallStateAt(row, col).isEmpty)
        Some((row, col))
      else
        pickRandomEmptyCell(rng, board) // loop: try again
    }
  }

  private[game] def placeInitialBalls(rng: Random, board: Board): Board = {
    val newBoard =
      (1 to 5).foldLeft(board) { //???? parameterize
        case (board2, _) =>
          val (row, column) =
            pickRandomEmptyCell(rng, board2)
                .getOrElse(scala.sys.error("Unexpectedly full board"))
          board2.withCellHavingBall(row, column, pickRandomBallKind(rng))
      }
    //?????? add 3 on-deck balls
    newBoard
  }

  private[game] sealed trait Action
  private[game] object Action {
    private[game] case object SelectBall  extends Action
    private[game] case object SelectEmpty extends Action
    private[game] case object Deselect    extends Action
    private[game] case object TryMoveBall extends Action
    private[game] case object Pass        extends Action
  }
  import Action._

  def interpretTapLocationToTapAction(board: Board, row: RowIndex, column: ColumnIndex): Action =
    tapAndStateToTapAction(hasABall           = board.hasABallSelected,
                           isSelectedAt       = board.isSelectedAt(row, column),
                           hasABallSelected   = board.hasABallSelected,
                           hasAnyCellSelected = board.hasAnyCellSelected)

  private def tapAndStateToTapAction(hasABall: Boolean,
                                     isSelectedAt: Boolean,
                                     hasABallSelected: Boolean,
                                     hasAnyCellSelected: Boolean
                                    ): Action = {
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

    val onBallOrEmpty = if (hasABall) OnBall else OnEmpty
    val onSelOrUnsel  = if (isSelectedAt) OnSel else OnUnsel
    val hadBallOrNot  = if (hasABallSelected) HadBall else NoBall
    val hadSelOrNot   = if (hasAnyCellSelected) HadSel else NoSel

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

  private[game] def doPass(rng: Random, board: Board): Board = {
    //?????? scatter from real on-deck list, and replenish it too
    val onDeckListPlaceholder = List.fill(3)(pickRandomBallKind(rng))
    val newBoard =
      onDeckListPlaceholder
        .foldLeft(board) {
          case (board2, _) =>
            pickRandomEmptyCell(rng, board2) match {
              case None => board2
              case Some((row, column)) =>
                board2.withCellHavingBall(row, column, pickRandomBallKind(rng))
            }
        }
    newBoard
  }

}

