package com.us.dsb.explore.algs.ttt.manual

import cats.syntax.option._
import cats.syntax.either._
import enumeratum.EnumEntry



object Board {
  /*@newtype deferred*/ private case class Cell(state: Option[Player]) { // if newType, has to be in object
  }

  private object Cell {
    val empty: Cell = Cell(None)
  }

  def initial: Board = new Board(Vector.fill[Cell](3 * 3)(Cell.empty))
}

import Board._

// probably wrap in a GameState with currentPlayer (moved from GameUiState)
class Board(private val cellArray: Vector[Cell]) {

  private def vectorIndex(row: RowIndex, column: ColumnIndex): Int = {
    (row.value.value - 1) * 3 + (column.value - 1)
  }

  private def getCellAt(row: RowIndex, column: ColumnIndex): Cell = {
    cellArray(vectorIndex(row, column))
  }

  private def isEmptyAt(row: RowIndex, column: ColumnIndex): Boolean = {
    cellArray(vectorIndex(row, column)).state.isEmpty
  }

  def tryMoveAt(player: Player, row: RowIndex, column: ColumnIndex): Either[String, Board] = {
    if (isEmptyAt(row, column)) {
      val newCellArray = cellArray.updated(vectorIndex(row, column), Cell(player.some))
      val newBoard = new Board(newCellArray)
      newBoard.asRight
    }
    else {
      (s"Can't move at row $row, column $column;" +
          s" is already marked ${getCellAt(row, column)}").asLeft // ???? clean getCellAt, etc.
    }
  }

  def renderMultiline: String = {
    // ?? clean:  make more convenient; maybe provide ranges/iteration?
    // ?? move rowIndices/etc. to Board or index types
    val rowIndices = (1 to 3).map(x => RowIndex(Index.unsafeFrom(x)))  // ?? ~unsafe
    val columnIndices = (1 to 3).map(ColumnIndex)
    val cellWidth = 3
    val cellSeparator = "|"
    val wholeWidth =
      columnIndices.length * cellWidth +
          (columnIndices.length - 1) * cellSeparator.length
    val rowSeparator = "\n" + ("=" * wholeWidth) + "\n"

    rowIndices.map { row =>
      columnIndices.map { column =>
        getCellAt(row, column).state match {
          case None         => " - "
          case Some(player) => " " + player.toString + " "
        }
      }.mkString(cellSeparator)
    }.mkString(rowSeparator)
  }

}

