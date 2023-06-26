package com.us.dsb.explore.algs.coloredlines.manual.ui

import com.us.dsb.colorlines.game.board.{
  BallColor, BallColorRenderingExtensions, CellAddress, CellState, ColumnIndex, Index, RowIndex}

import scala.annotation.unused

// ????? TODO: Clarify names (e.g., GameUIState vs. TapUiGameState)--
//  virtual tap-level UI vs. text-controlled selection-based ~simulation of taps

// ?? somewhere expand to allow for history (maybe via Semigroup or whatever has .compose?)
private case class GameUIState(tapUiGameState: TapUiGameState,
                                     cursorAddress: CellAddress) {

  // ?? clean up that floorMod; I just want plain mathematical mod:
  private def adjustAndWrapToRange(unincremented: Index, delta: Int): Index = {
    //????? test (at least callers)
    // ???? TODO:  Review re Index members/etc."
    val indexOrigin = Index.MinValue.value
    val rangeSize = Index.MaxValue.value - Index.MinValue.value + 1
    val rawIncremented = unincremented.value + delta
    Index.unsafeFrom(
      scala.math.floorMod(rawIncremented - indexOrigin, rangeSize)
          + indexOrigin)
  }

  // ?? should UI work directly with board's index types, or should it
  //   use its own (maybe just to simulate ...)?
  // ?? who should do/provide this index-increment logic? (it's just for
  //   our cursor-based row/column specification; what would GUI use, just
  //   9 table-level IDs tied to GUI cells/buttons?);

  // ???? TODO:  "adjusted"? "offset"?
  private[ui] def withRowAdjustedBy(delta: Int): GameUIState = {
    val adjustedRow = RowIndex(adjustAndWrapToRange(cursorAddress.row.raw, delta))
    copy(cursorAddress = cursorAddress.copy(row = adjustedRow))
  }

  private[ui] def withColumnAdjustedBy(delta: Int): GameUIState = {
    val adjustedColumn = ColumnIndex(adjustAndWrapToRange(cursorAddress.column.raw, delta))
    copy(cursorAddress = cursorAddress.copy(column = adjustedColumn))
  }

  /** Gets full cell-state string.  (For cell state plus tap-selection state;
   *  character wrapped in ANSI text color escape sequences.) */
  private[manual] def getCellStateChar(callState: CellState,
                                       isSelected: Boolean): String = {
    import BallColorRenderingExtensions.*
    callState.asOption match {
      case Some(ball) => ball.getColoredCharSeq(isSelected)
      case None       => if (! isSelected) "-" else "@"
    }
  }

  private def renderTableMultilineWithSelection: String = {
    val cellWidth = " X ".length
    val cellSeparator = "|"
    // ?? use new Order or leave using indices declarations?
    val wholeWidth =
      ColumnIndex.values.size * cellWidth +
          (ColumnIndex.values.size - 1) * cellSeparator.length
    val rowSeparator = "\n" + ("-" * wholeWidth) + "\n"

    RowIndex.values.map { row =>
      ColumnIndex.values.map { column =>
        val scanAddress = CellAddress(row, column)
        val tapCellStateStr =
          getCellStateChar(tapUiGameState.gameState.board.getCellStateAt(scanAddress),
                           tapUiGameState.isSelectedAt(scanAddress))
        val fullCellStateStr =
          if (scanAddress == cursorAddress ) {
            "*" + tapCellStateStr + "*"
          }
          else {
            " " + tapCellStateStr + " "
          }
        fullCellStateStr
      }.mkString(cellSeparator)  // make each row line
    }.mkString(rowSeparator)     // make whole-board multi-line string
  }

  // ?? Unused as of 2023-06-08; previously in LowerGameState.
  @unused
  private def renderCompactTableMultilineWithSelection(selectionAddress: Option[CellAddress]): String = {
    RowIndex.values.map { row =>
      ColumnIndex.values.map { column =>
        val addr = CellAddress(row, column)
        val isSelected = selectionAddress.fold(false)(_ == addr)
        getCellStateChar(tapUiGameState.gameState.board.getCellStateAt(addr), isSelected)
      }.mkString("|")  // make each row line
    }.mkString("\n")   // make whole-board multi-line string
  }

  private[ui] def toDisplayString: String = {
    import BallColorRenderingExtensions.*
    val ondeckList =
      tapUiGameState.gameState.board.getOndeckBalls
          .map(_.getColoredCharSeq(forBackground = false))
          .mkString(", ")

    renderTableMultilineWithSelection + "\n" +
        s"Next: $ondeckList" +
        s"  Score: ${tapUiGameState.gameState.getScore}" +
        s"  Marking cursor: <row ${cursorAddress.row} / column ${cursorAddress.column}>"
  }

}

