package com.us.dsb.explore.algs.coloredlines.manual.game

import cats.syntax.option._
import cats.syntax.either._
import com.us.dsb.explore.algs.coloredlines.manual.game.{BoardOrder, ColumnIndex, RowIndex}
import enumeratum.{Enum, EnumEntry}

import scala.io.AnsiColor

private[manual] object Board {

  /** Empty or ball of some color, plus marked or not for physical move. */
  case class CellState(ballState: Option[BallKind],
                  isSelected: Boolean)
  private object CellState {
    val empty: CellState = CellState(None, false)
  }

  private[game] def empty: Board =
    new Board(Vector.fill[CellState](BoardOrder * BoardOrder)(CellState.empty))
}

import Board._

/**
 * State of TTT board (just cells; not whose turn it is/etc.)
 */
private[game] class Board(private val cellStates: Vector[CellState]) {

  /** Computes row-major cell-array index from row and column numbers. */
  private def vectorIndex(row: RowIndex, column: ColumnIndex): Int =
    (row.value.value - 1) * BoardOrder + (column.value.value - 1)

  def getCellStateAt(row: RowIndex, column: ColumnIndex): CellState = {
    cellStates(vectorIndex(row, column))
  }

  def isFull: Boolean = ! cellStates.exists(_.ballState.isEmpty)

  def getBallStateAt(row: RowIndex, column: ColumnIndex): Option[BallKind] = {
    cellStates(vectorIndex(row, column)).ballState
  }
  def hasABallAt(row: RowIndex, column: ColumnIndex): Boolean = {
    cellStates(vectorIndex(row, column)).ballState.isDefined
  }
  def isSelectedAt(row: RowIndex, column: ColumnIndex): Boolean = {
    cellStates(vectorIndex(row, column)).isSelected
  }
  def hasABallSelected: Boolean =
    cellStates.find(_.isSelected).exists(_.ballState.isDefined)

  def hasAnyCellSelected: Boolean = cellStates.exists(_.isSelected)

  // (Maybe less private in future.)
  private def zzwithCellState(row: RowIndex,
                              column: ColumnIndex,
                              newState: CellState): Board =
    new Board(cellStates.updated(vectorIndex(row, column), newState))

  def withCellHavingBall(row: RowIndex,
                         column: ColumnIndex,
                         ball: BallKind): Board =
    zzwithCellState(row, column, getCellStateAt(row, column).copy(ballState = Some(ball)))

  def withCellSelected(row: RowIndex,
                       column: ColumnIndex): Board =
    withNoSelection.zzwithCellState(row, column, getCellStateAt(row, column).copy(isSelected = true))

  def withNoSelection: Board =
    new Board(cellStates.map(c => c.copy(isSelected = false)))

  /*
    getting (multiple) lines of 5 given a cell (with a ball)
    - consider 4 axes (N, NE, E, SE)
    - consider 2 directions per axois
    - for each axis, get number of same-color balls in a row:
      - for each direction
        - keep moving out while next position exists and have ball of same color (and
        - ~combine direction results to get length of line in axes (1 to 9 balls)
    - if no axis row has 5 or more:  place 3 on-deck balls and select next 3
    - if any has 5 or more:
      - for each with five or more:
        - assimilate length into move score (total 5 -> 10 pt, +1 -> +4 pt;  (4 * N - 10)
        - remove balls from cells (watch overlap)
   */

  def getStateChar(state: CellState): String = {  //???? move out
    state.ballState match {
      case Some(ball) => ball.getColoredCharSeq(state.isSelected)
      case None => if (! state.isSelected) "-" else "@"
    }
  }

  /** Makes compact single-line string like Xx"<X-O/-X-/O-X>". */
  override def toString: String = {
    rowIndices.map { row =>
      columnIndices.map { column =>
        getStateChar(getCellStateAt(row, column))
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
          "" + getStateChar(getCellStateAt(row, column)) + " "
      }.mkString(cellSeparator)  // make each row line
    }.mkString(rowSeparator)     // make whole-board multi-line string
  }

  def renderCompactMultiline: String = {
    rowIndices.map { row =>
      columnIndices.map { column =>
        getStateChar(getCellStateAt(row, column))
      }.mkString("|")  // make each row line
    }.mkString("\n")
  }

}

