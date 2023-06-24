package com.us.dsb.explore.algs.coloredlines.manual.ui.tapapi

import com.us.dsb.colorlines.game.board.CellAddress
import com.us.dsb.explore.algs.coloredlines.manual.ui.TapUiGameState

object TapInterpreter {

  /** Interprets location of (virtual) tap, given tap API state (tap-level cell
   *  selection), to specific case (tap state change or game action). */
  def interpretTapLocationToTapCase(tapUiState: TapUiGameState,
                                    address   : CellAddress): TapCase =
    tapAndStateToTapCase(onABall            = tapUiState.gameState.board.hasABallAt(address),
                         isSelectedAt       = tapUiState.isSelectedAt(address),
                         hasABallSelected   = tapUiState.hasABallSelected,
                         hasAnyCellSelected = tapUiState.hasAnyCellSelected)

  private def tapAndStateToTapCase(onABall           : Boolean,
                                   isSelectedAt      : Boolean,
                                   hasABallSelected  : Boolean,
                                   hasAnyCellSelected: Boolean
                                  ): TapCase = {
    object RenameOrFlattenThis { // grouped/nested re debugger clutter
      sealed trait OnBallOrEmpty derives CanEqual
      case object OnBall  extends OnBallOrEmpty
      case object OnEmpty extends OnBallOrEmpty

      sealed trait OnSelOrUnsel derives CanEqual
      case object OnSel   extends OnSelOrUnsel
      case object OnUnsel extends OnSelOrUnsel

      sealed trait HadBallOrNot derives CanEqual
      case object HadBall extends HadBallOrNot
      case object NoBall  extends HadBallOrNot

      sealed trait HadSelOrNot derives CanEqual
      case object HadSel extends HadSelOrNot
      case object NoSel  extends HadSelOrNot
    }
    import RenameOrFlattenThis.*

    val onBallOrEmpty = if (onABall)            OnBall  else OnEmpty
    val onSelOrUnsel  = if (isSelectedAt)       OnSel   else OnUnsel
    val hadBallOrNot  = if (hasABallSelected)   HadBall else NoBall
    val hadSelOrNot   = if (hasAnyCellSelected) HadSel  else NoSel

    val tapCase: TapCase = {
      val conditions = (onBallOrEmpty, hadBallOrNot, onSelOrUnsel, hadSelOrNot)
      import TapCase.*
      conditions match {
        case (OnBall,  _,       OnUnsel, _     ) => SelectBallTap    // - tap on ball  when unselected
        case (OnEmpty, HadBall, _,       _     ) => TryMoveBallTap   // - tap on empty when ball selected

        case (OnEmpty, NoBall,  OnUnsel, NoSel ) => SelectEmptyTap   // - tap on empty when no selection
        case (OnEmpty, NoBall,  OnUnsel, HadSel) => PassTap          // - tap on empty when other empty selected

        case (_,       _,       OnSel,   _     ) => DeselectTap      // - tap on either when selected
      }
    }
    tapCase
  }

}
