package com.us.dsb.colorlines.game.board

import com.us.dsb.colorlines.game.board.{
  ColumnIndex, Index, IndexOrigin, RowIndex, rowIndices, columnIndices}

/** Valid (in-board) cell address.  (Re private constructor, see `CellAddress.apply`.) */
case class CellAddress private(row: RowIndex,
                               column: ColumnIndex) derives CanEqual {
  /** Like normal `copy` method but returns re-used instances from `CellAddress.apply` */
  def copy(row: RowIndex = this.row,
           column: ColumnIndex = this.column): CellAddress = {
    CellAddress(row, column)
  }
}

object CellAddress {

  private val instances: IndexedSeq[IndexedSeq[CellAddress]] =
    rowIndices.map { rowIndex =>
        columnIndices.map { colIndex =>
          new CellAddress(rowIndex, colIndex)
        }
    }

  /** Logical constructor but returns re-used instances. */
  def apply(row: RowIndex, column: ColumnIndex): CellAddress =
    instances(row.raw.value - IndexOrigin)(column.raw.value - IndexOrigin)

  /** Constructs cell address from raw index Int values (not offsets). */
  def fromRaw(rawRowIndex: Int, rawColumnIndex: Int) =
    CellAddress(RowIndex(Index.unsafeFrom(rawRowIndex)),
                ColumnIndex(Index.unsafeFrom(rawColumnIndex)))
}

