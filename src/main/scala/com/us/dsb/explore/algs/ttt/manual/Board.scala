package com.us.dsb.explore.algs.ttt.manual

import cats.syntax.option._
import cats.syntax.either._
import enumeratum.EnumEntry



object Board {
  /** Empty, X, or O */
  /*@newtype deferred*/
  private case class Cell(state: Option[Player]) { // if newType, has to be in object
  }

  private object Cell {
    val empty: Cell = Cell(None)
  }

  def initial: Board = new Board(Vector.fill[Cell](Order * Order)(Cell.empty))
}

import Board._

// probably wrap in a GameState with currentPlayer (moved from GameUiState)
class Board(private val cellStates: Vector[Cell]) {

  /** Maps logical row/column to row-major vector index. */
  private def vectorIndex(row: RowIndex, column: ColumnIndex): Int =
    (row.value.value - 1) * Order + (column.value.value - 1)

  // ?? change to public for realistic UI access
  private def getCellAt(row: RowIndex, column: ColumnIndex): Cell =
    cellStates(vectorIndex(row, column))

  def getMarkAt(row: RowIndex, column: ColumnIndex): Option[Player] = {
    getCellAt(row, column).state
  }

  // ?? later refine from Either[String, ...] to "fancier" error type
  def tryMoveAt(player: Player,
                row: RowIndex,
                column: ColumnIndex): Either[String, Board] = {
    getCellAt(row, column).state match {
      case None =>
        val newCellArray = cellStates.updated(vectorIndex(row, column), Cell(player.some))
        val newBoard = new Board(newCellArray)
        newBoard.asRight

      case Some(nameThis) =>
        (s"Can't place mark at row $row, column $column;" +
            s" is already marked (${nameThis})").asLeft
    }
  }

  def renderMultiline: String = {
    val cellWidth = " X ".length
    val cellSeparator = "|"
    // ?? user new Order or leave using indices declarations?
    val wholeWidth =
      columnIndices.length * cellWidth +
          (columnIndices.length - 1) * cellSeparator.length
    val rowSeparator = "\n" + ("-" * wholeWidth) + "\n"

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

