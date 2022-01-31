package com.us.dsb.explore.algs.ttt.manual.game

import cats.syntax.option._
import cats.syntax.either._


private[manual] object Board {

  /** Empty, player-X mark, or player-O mark. */
  private case class Cell(state: Option[Player])
  private object Cell {
    val empty: Cell = Cell(None)
  }

  private[game] def initial: Board =
    new Board(Vector.fill[Cell](Order * Order)(Cell.empty))
}

import Board._

/**
 * State of TTT board (just cells; not whose turn it is/etc.)
 */
private[game] class Board(private val cellStates: Vector[Cell]) {

  /** Computes row-major cell-array index from row and column numbers. */
  private def vectorIndex(row: RowIndex, column: ColumnIndex): Int =
    (row.value.value - 1) * Order + (column.value.value - 1)

  def getMarkAt(row: RowIndex, column: ColumnIndex): Option[Player] = {
    cellStates(vectorIndex(row, column)).state
  }

  // (Maybe less private in future.)
  private def withCellUpdated(row: RowIndex,
                              column: ColumnIndex,
                              cellState: Option[Player]): Board = {
    new Board(cellStates.updated(vectorIndex(row, column), Cell(cellState)))
  }

  def withCellMarkedForPlayer(row: RowIndex,
                              column: ColumnIndex,
                              player: Player): Board = {
    withCellUpdated(row, column, player.some)
  }

  // Not for basic TTT game; for backtracking/undo.
  def withCellUnmarked(row: RowIndex,
                       column: ColumnIndex): Board = {
    withCellUpdated(row, column, None)
  }

  def hasNoMovesLeft: Boolean = ! cellStates.exists(_.state.isEmpty)


  // ?? Q: How to use Tuple3 in data and then convert to or use as 3-element
  //   List and call .forall?
  // ?? any way auto-generate this _simply_?  or factor out repeated cell pairs?
  private type CellRawIndices = (Int, Int)
  private val linesData: List[(CellRawIndices, CellRawIndices, CellRawIndices)] =
        List(
          ((1, 1), (1, 2), (1, 3)),
          ((2, 1), (2, 2), (2, 3)),
          ((3, 1), (3, 2), (3, 3)),
          ((1, 1), (2, 1), (3, 1)),
          ((1, 2), (2, 2), (3, 2)),
          ((1, 3), (2, 3), (3, 3)),
          ((1, 1), (2, 2), (3, 3)),
          ((1, 3), (2, 2), (3, 1)))

  def hasThreeInARow: Boolean = {
    // See if there _exists_ line where _all_ cells are marked by the same player:
    linesData.exists { case (cell1, cell2, cell3) =>
      val lineCellValues =
        List(cell1, cell2, cell3) .map { case (row, column) =>
              getMarkAt(RowIndex(Index.unsafeFrom(row)),
                        ColumnIndex(Index.unsafeFrom(column)))
        }
      // if first not None and all same as that first one:
      // compare all to first (if not None)
      val firstCellState = lineCellValues.head  // .get -- list size > 0
      firstCellState.isDefined && lineCellValues.forall(_ == firstCellState)
    }
  }

  /** Makes compact single-line string like "<X-O/-X-/O-X>". */
  override def toString: String = {
    rowIndices.map { row =>
      columnIndices.map { column =>
        getMarkAt(row, column) match {
          case None         => "-"
          case Some(player) => player.toString
        }
      }.mkString("")
    }.mkString("<", "/", ">")
  }

  def renderMultiline: String = {
    val cellWidth = " X ".length
    val cellSeparator = "|"
    val wholeWidth =
      columnIndices.length * cellWidth +
          (columnIndices.length - 1) * cellSeparator.length
    val rowSeparator = "\n" + ("-" * wholeWidth) + "\n"

    rowIndices.map { row =>
      columnIndices.map { column =>
        getMarkAt(row, column) match {
          case None         => " - "
          case Some(player) => " " + player.toString + " "
        }
      }.mkString(cellSeparator)  // make each row line
    }.mkString(rowSeparator)     // make whole-board multi-line string
  }

  def renderCompactMultiline: String = {
    rowIndices.map { row =>
      columnIndices.map { column =>
        getMarkAt(row, column) match {
          case None         => "-"
          case Some(player) => player.toString
        }
      }.mkString("|")  // make each row line
    }.mkString("\n")
  }

}

