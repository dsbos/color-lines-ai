package com.us.dsb.explore.algs.coloredlines.manual.ui

import com.us.dsb.explore.algs.coloredlines.manual.game._
import com.us.dsb.explore.algs.coloredlines.manual.game.{ColumnIndex, Index, RowIndex}

// ?? somewhere expand to allow for history (maybe via Semigroup or whatever has .compose?)
private[this] case class GameUIState(gameState: GameState,
                                     selectedRow: RowIndex,
                                     selectedColumn: ColumnIndex) {

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

  private[ui] def withRowAdjustedBy(delta: Int): GameUIState =
    copy(selectedRow = RowIndex(adjustAndWrapToRange(selectedRow.value, delta)))

  private[ui] def withColumnAdjustedBy(delta: Int): GameUIState =
    copy(selectedColumn = ColumnIndex(adjustAndWrapToRange(selectedColumn.value, delta)))

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
        val cellStateStr =
          gameState.board.getStateChar(gameState.board.getCellStateAt(row, column))
        if (row == selectedRow && column == selectedColumn ) {
          "*" + cellStateStr + "*"
        }
        else {
          " " + cellStateStr + " "
        }
      }.mkString(cellSeparator)  // make each row line
    }.mkString(rowSeparator)     // make whole-board multi-line string

  }

  private[ui] def toDisplayString: String = {
    renderTableMultilineWithSelection + "\n" +
    s"XxTurn: Marking cursor: <row $selectedRow / column $selectedColumn>"
  }

}

