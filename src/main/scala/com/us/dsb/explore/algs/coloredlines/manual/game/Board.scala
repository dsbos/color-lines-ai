package com.us.dsb.explore.algs.coloredlines.manual.game

import cats.syntax.option._
import cats.syntax.either._
import com.us.dsb.explore.algs.coloredlines.manual.game.{BoardOrder, ColumnIndex, RowIndex}
import enumeratum.{Enum, EnumEntry}

import scala.io.AnsiColor


private[manual] object Board {

  sealed class BallKind(val initial: String,
                        val setFgColorSeq: String,
                        val setBgColorSeq: String
                       ) extends EnumEntry {
    def getColoredCharSeq(background: Boolean): String =
      (if (background) this.setBgColorSeq else this.setFgColorSeq) +
          initial +
          AnsiColor.RESET
  }

  object BallKind extends Enum[BallKind] {
    // original: blue.dark, blue.light, brown, green, purple, red, yellow
    case object Blue    extends BallKind("b",  AnsiColor.BLUE,    AnsiColor.BLUE_B)
    case object Cyan    extends BallKind("c",  AnsiColor.CYAN,    AnsiColor.CYAN_B)
    case object Black   extends BallKind("k",  AnsiColor.BLACK,   AnsiColor.BLACK_B)
    case object Green   extends BallKind("g",  AnsiColor.GREEN,   AnsiColor.GREEN_B)
    case object Magenta extends BallKind("m",  AnsiColor.MAGENTA, AnsiColor.MAGENTA_B)
    case object Red     extends BallKind("r",  AnsiColor.RED,     AnsiColor.RED_B)
    case object Yellow  extends BallKind("y",  AnsiColor.YELLOW,  AnsiColor.YELLOW_B)

    override val values: IndexedSeq[BallKind] = findValues
  }


  /** Empty or ball of some color, plus marked or not for physical move. */
  /*private*/ case class CellState(ballState: Option[BallKind],
                                   isSelected: Boolean)
  private object CellState {
    val empty: CellState = CellState(None, false)
  }

  private case class Cell(state: CellState)  //???? coalesce Cell and CellState
  private object Cell {
    val empty: Cell = Cell(CellState.empty)
  }

  private[game] def empty: Board =
    new Board(Vector.fill[Cell](BoardOrder * BoardOrder)(Cell.empty))
}

import Board._

/**
 * State of TTT board (just cells; not whose turn it is/etc.)
 */
private[game] class Board(private val cellStates: Vector[Cell]) {


  /** Computes row-major cell-array index from row and column numbers. */
  private def vectorIndex(row: RowIndex, column: ColumnIndex): Int =
    (row.value.value - 1) * BoardOrder + (column.value.value - 1)

  def getStateAt(row: RowIndex, column: ColumnIndex): CellState = {
    cellStates(vectorIndex(row, column)).state
  }

  def isFull: Boolean = ! cellStates.exists(_.state.ballState.isEmpty)

  def getBallStateAt(row: RowIndex, column: ColumnIndex): Option[BallKind] = {
    cellStates(vectorIndex(row, column)).state.ballState
  }
  def hasABallAt(row: RowIndex, column: ColumnIndex): Boolean = {
    cellStates(vectorIndex(row, column)).state.ballState.isDefined
  }
  def isSelectedAt(row: RowIndex, column: ColumnIndex): Boolean = {
    cellStates(vectorIndex(row, column)).state.isSelected
  }
  def hasABallSelected: Boolean =
    cellStates.find(_.state.isSelected).exists(_.state.ballState.isDefined)

  def hasAnyCellSelected: Boolean = cellStates.exists(_.state.isSelected)



  // (Maybe less private in future.)
  private def withCellState(row: RowIndex,
                            column: ColumnIndex,
                            cellState: CellState): Board =
    new Board(cellStates.updated(vectorIndex(row, column), Cell(cellState)))

  def withCellHavingBall(row: RowIndex,
                          column: ColumnIndex,
                          ball: BallKind): Board =
    withCellState(row, column, getStateAt(row, column).copy(ballState = Some(ball)))

  def withCellSelected(row: RowIndex, column: ColumnIndex): Board =
    withNoSelection.withCellState(row, column, getStateAt(row, column).copy(isSelected = true))

  def withNoSelection: Board =
    new Board(cellStates.map(c => c.copy(state = c.state.copy(isSelected = false))))





  // ?? Q: How to use Tuple3 in data and then convert to or use as 3-element
  //   List and call .forall?
  // ?? any way auto-generate this _simply_?  or factor out repeated cell pairs?
  private type XxCellRawIndices = (Int, Int)
  private val XxxlinesData: List[(XxCellRawIndices, XxCellRawIndices, XxCellRawIndices)] =
        List(
          ((1, 1), (1, 2), (1, 3)),
          ((2, 1), (2, 2), (2, 3)),
          ((3, 1), (3, 2), (3, 3)),
          ((1, 1), (2, 1), (3, 1)),
          ((1, 2), (2, 2), (3, 2)),
          ((1, 3), (2, 3), (3, 3)),
          ((1, 1), (2, 2), (3, 3)),
          ((1, 3), (2, 2), (3, 1)))


//  def xxXxxhasThreeInARow: Boolean = {
//    // See if there _exists_ line where _all_ cells are marked by the same player:
//    XxxlinesData.exists { case (cell1, cell2, cell3) =>
//      val lineCellValues =
//        List(cell1, cell2, cell3) .map { case (row, column) =>
//              XxgetMarkAt(RowIndex(Index.unsafeFrom(row)),
//                        ColumnIndex(Index.unsafeFrom(column)))
//        }
//      // if first not None and all same as that first one:
//      // compare all to first (if not None)
//      val firstCellState = lineCellValues.head  // .get -- list size > 0
//      firstCellState.isDefined && lineCellValues.forall(_ == firstCellState)
//    }
//  }

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

  def xxgetStateChar(state: CellState): String = {  //???? move out
    state.ballState match {
      case Some(ball) => ball.getColoredCharSeq(state.isSelected)
      case None => if (! state.isSelected) "-" else "@"
    }
  }

  /** Makes compact single-line string like Xx"<X-O/-X-/O-X>". */
  override def toString: String = {
    rowIndices.map { row =>
      columnIndices.map { column =>
        xxgetStateChar(getStateAt(row, column))
      }.mkString("")
    }.mkString("<", "/", ">")
  }

  def xxrenderMultiline: String = {
    val cellWidth = " X ".length
    val cellSeparator = "|"
    val wholeWidth =
      columnIndices.length * cellWidth +
          (columnIndices.length - 1) * cellSeparator.length
    val rowSeparator = "\n" + ("-" * wholeWidth) + "\n"

    rowIndices.map { row =>
      columnIndices.map { column =>
          "" + xxgetStateChar(getStateAt(row, column)) + " "
      }.mkString(cellSeparator)  // make each row line
    }.mkString(rowSeparator)     // make whole-board multi-line string
  }

  def xxrenderCompactMultiline: String = {
    rowIndices.map { row =>
      columnIndices.map { column =>
        xxgetStateChar(getStateAt(row, column))
      }.mkString("|")  // make each row line
    }.mkString("\n")
  }

}

