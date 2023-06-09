package com.us.dsb.explore.algs.coloredlines.manual.ui

import com.us.dsb.explore.algs.coloredlines.manual.game.board.{
  BallColor, CellAddress, ColumnIndex, Index, RowIndex, columnIndices, rowIndices}
import com.us.dsb.explore.algs.coloredlines.manual.game._

// ?????? TODO: Clarify names (e.g., GameUIState vs. TapUiGameState)--
//  virtual tap-level UI vs. text-controlled selection-based ~simulation of taps

// ?? somewhere expand to allow for history (maybe via Semigroup or whatever has .compose?)
private[this] case class GameUIState(tapUiGameState: TapUiGameState,
                                     cursorAddress: CellAddress) {

  // ?? clean up that floorMod; I just want plain mathematical mod:
  private[this] def adjustAndWrapToRange(unincremented: Index, delta: Int): Index = {
    //????? test (at least callers)
    // ?? maybe enable auto-wrapping and -unwrapping around math
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
    val adjustedRow = RowIndex(adjustAndWrapToRange(cursorAddress.row.value, delta))
    copy(cursorAddress = cursorAddress.copy(row = adjustedRow))
  }

  private[ui] def withColumnAdjustedBy(delta: Int): GameUIState = {
    val adjustedColumn = ColumnIndex(adjustAndWrapToRange(cursorAddress.column.value, delta))
    copy(cursorAddress = cursorAddress.copy(column = adjustedColumn))
  }

  /** Gets full cell-state string.  (For cell state plus tap-selection state;
   *  character wrapped in ANSI text color escape sequences.) */
  private[manual] def getCellBallStateChar(ballState: Option[BallColor],
                                           isSelected: Boolean): String = {
    ballState match {
      case Some(ball) => ball.getColoredCharSeq(isSelected)
      case None       => if (! isSelected) "-" else "@"
    }
  }

  private[this] def renderTableMultilineWithSelection: String = {
    val cellWidth = " X ".length
    val cellSeparator = "|"
    // ?? use new Order or leave using indices declarations?
    val wholeWidth =
      columnIndices.length * cellWidth +
          (columnIndices.length - 1) * cellSeparator.length
    val rowSeparator = "\n" + ("-" * wholeWidth) + "\n"

    rowIndices.map { row =>
      columnIndices.map { column =>
        val scanAddress = CellAddress(row, column)
        val tapCellStateStr =
          getCellBallStateChar(tapUiGameState.gameState.board.getBallStateAt(scanAddress),
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
  private[this] def renderCompactTableMultilineWithSelection(selectionAddress: Option[CellAddress]): String = {
    rowIndices.map { row =>
      columnIndices.map { column =>
        val addr = CellAddress(row, column)
        val isSelected = selectionAddress.fold(false)(_ == addr)
        getCellBallStateChar(tapUiGameState.gameState.board.getBallStateAt(addr), isSelected)
      }.mkString("|")  // make each row line
    }.mkString("\n")   // make whole-board multi-line string
  }

  private[ui] def toDisplayString: String = {
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

