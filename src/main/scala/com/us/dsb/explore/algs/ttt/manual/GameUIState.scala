package com.us.dsb.explore.algs.ttt.manual

// ?? somewhere expand to allow for history (maybe via Semigroup or whatever has .compose?)
case class GameUIState(gameState: GameState,
                       selectedRow: RowIndex,
                       selectedColumn: ColumnIndex) {

  // ?? clean up that floorMod; I just want plain mathematical mod:
  private def adjustAndwrapToRange(unincremented: Index, delta: Int): Index = {
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

  def withRowAdustedBy(delta: Int): GameUIState =
    copy(selectedRow = RowIndex(adjustAndwrapToRange(selectedRow.value, delta)))

  def withColumnAdustedBy(delta: Int): GameUIState =
    copy(selectedColumn = ColumnIndex(adjustAndwrapToRange(selectedColumn.value, delta)))

  private def renderTableMultilineWithSelection: String = {
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
          gameState.board.getMarkAt(row, column) match {
            case None => "-"
            case Some(player) => player.toString
          }
        if (row == selectedRow && column == selectedColumn ) {
          "*" + cellStateStr + "*"
        }
        else {
          " " + cellStateStr + " "
        }
      }.mkString(cellSeparator)  // make each row line
    }.mkString(rowSeparator)     // make whole-board multi-line string

  }

  def toDisplayString: String = {
    renderTableMultilineWithSelection + "\n" +
    s"Turn: Player ${gameState.currentPlayer}; marking cursor: <row $selectedRow / column $selectedColumn>"
  }

}

