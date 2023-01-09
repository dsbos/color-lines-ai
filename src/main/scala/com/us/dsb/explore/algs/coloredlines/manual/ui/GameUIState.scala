package com.us.dsb.explore.algs.coloredlines.manual.ui

import com.us.dsb.explore.algs.coloredlines.manual.game.board.CellAddress
import com.us.dsb.explore.algs.coloredlines.manual.game._
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{ColumnIndex, Index, RowIndex, columnIndices, rowIndices}

// ?? somewhere expand to allow for history (maybe via Semigroup or whatever has .compose?)
private[this] case class GameUIState(gameState: UpperGameState,
                                     cursorAddress: CellAddress) {

  // ?? clean up that floorMod; I just want plain mathematical mod:
  private[this] def adjustAndWrapToRange(unincremented: Index, delta: Int): Index = {
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

  private[ui] def withRowAdjustedBy(delta: Int): GameUIState = {
    val adjustedRow = RowIndex(adjustAndWrapToRange(cursorAddress.row.value, delta))
    copy(cursorAddress = cursorAddress.copy(row = adjustedRow))
  }

  private[ui] def withColumnAdjustedBy(delta: Int): GameUIState = {
    val adjustedColumn = ColumnIndex(adjustAndWrapToRange(cursorAddress.column.value, delta))
    copy(cursorAddress = cursorAddress.copy(column = adjustedColumn))
  }

  //?????? add on-deck balls

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
        val cellStateStr =
          gameState.boardPlus.getCellBallStateChar(gameState.boardPlus.getBallStateAt(scanAddress),
                                               gameState.boardPlus.isSelectedAt(scanAddress))
        if (scanAddress == cursorAddress ) {
          "*" + cellStateStr + "*"
        }
        else {
          " " + cellStateStr + " "
        }
      }.mkString(cellSeparator)  // make each row line
    }.mkString(rowSeparator)     // make whole-board multi-line string

  }

  private[ui] def toDisplayString: String = {
    val onDeckList =
      gameState.boardPlus.boardState.getOnDeckBalls.map(_.getColoredCharSeq(false)).mkString(", ")

    renderTableMultilineWithSelection + "\n" +
        s"Next: $onDeckList" +
        s"  Score: ${gameState.boardPlus.getScore}" +
        s"  Marking cursor: <row ${cursorAddress.row} / column ${cursorAddress.column}>"
  }

}

