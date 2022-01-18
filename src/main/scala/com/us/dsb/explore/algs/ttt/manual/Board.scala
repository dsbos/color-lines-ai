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

  def noMovesLeft(): Boolean = {
    ! cellStates.exists(_.state.isEmpty)
  }

  /*private*/ def hasThreeInARow(player: Player): Boolean = {
    // ?? Q: How to use Tuple3 in data convert to or use as 3-element List
    //   and call .forall?
    // ?? auto-generate this
    type CellRawIndices = Tuple2[Int, Int]
    val linesData: List[Tuple3[CellRawIndices, CellRawIndices, CellRawIndices]] =
      List(
        ((1, 1), (1, 2), (1, 3)),
        ((2, 1), (2, 2), (2, 3)),
        ((3, 1), (3, 2), (3, 3)),
        ((1, 1), (2, 1), (3, 1)),
        ((1, 2), (2, 2), (3, 2)),
        ((1, 3), (2, 3), (3, 3)),
        ((1, 1), (2, 2), (3, 3)),
        ((1, 3), (2, 2), (3, 1))
    )
    linesData.exists { case (cell1, cell2, cell3) =>
      val lineList = List(cell1, cell2, cell3)
      lineList.forall { case (row, column) =>

        player.some ==
            getCellAt(RowIndex(Index.unsafeFrom(row)),
                      ColumnIndex(Index.unsafeFrom(column)))
                .state
        }
    }
  }

  def markCell(player: Player,
               row: RowIndex,
               column: ColumnIndex): Either[String, Board] = {
    getCellAt(row, column).state match {
      case None =>
        val newCellArray =
          cellStates.updated(vectorIndex(row, column), Cell(player.some))
        new Board(newCellArray).asRight
      case Some(nameThis) =>
        (s"Can't place mark at row $row, column $column;" +
            s" is already marked (${nameThis})").asLeft    }
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

