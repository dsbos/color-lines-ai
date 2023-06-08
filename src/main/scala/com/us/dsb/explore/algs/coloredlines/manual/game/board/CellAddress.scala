package com.us.dsb.explore.algs.coloredlines.manual.game.board

/** Valid (in-board) cell address */
private[manual] case class CellAddress(row: RowIndex, column: ColumnIndex)

private[manual] object CellAddress {

  /** Constructs cell address from raw index integers (not offsets). */
  private[manual] def fromRaw(rawRowIndex: Int, rawColumnIndex: Int) =
    CellAddress(RowIndex(Index.unsafeFrom(rawRowIndex)),
                ColumnIndex(Index.unsafeFrom(rawColumnIndex)))
}

