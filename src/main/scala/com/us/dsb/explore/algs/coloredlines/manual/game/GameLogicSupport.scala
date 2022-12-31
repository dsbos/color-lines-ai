package com.us.dsb.explore.algs.coloredlines.manual.game

object GameLogicSupport {

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

}

