package com.us.dsb.explore.algs.coloredlines.manual.ui

import com.us.dsb.explore.algs.coloredlines.manual.game.board.CellAddress

// ???????? TODO:  Probably split virtual tap level from text-control level.
object TapUiSupport {

  /** Interpreted meaning/command of a (virtual) tap on a board cell. */
  private[ui] sealed trait TapAction

  private[ui] object TapAction {

    /** Select tapped-on cell that has ball. */
    private[ui] case object SelectBall  extends TapAction

    /** Select tapped-on cell that is empty. */
    private[ui] case object SelectEmpty extends TapAction

    /** Cancel selection. */
    private[ui] case object Deselect    extends TapAction

    /** (Try to) move ball from selected cell to tapped-on empty cell (if open path). */
    private[ui] case object TryMoveBall extends TapAction  //???? should this capture, carry coords?

    /** Pass (move nothing, get more balls placed). */
    private[ui] case object Pass        extends TapAction
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
      case object OnBall  extends OnBallOrEmpty
      case object OnEmpty extends OnBallOrEmpty

      sealed trait OnSelOrUnsel
      case object OnSel   extends OnSelOrUnsel
      case object OnUnsel extends OnSelOrUnsel

      sealed trait HadBallOrNot
      case object HadBall extends HadBallOrNot
      case object NoBall  extends HadBallOrNot

      sealed trait HadSelOrNot
      case object HadSel extends HadSelOrNot
      case object NoSel  extends HadSelOrNot
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

}
