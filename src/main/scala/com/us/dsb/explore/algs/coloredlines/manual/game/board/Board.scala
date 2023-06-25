package com.us.dsb.explore.algs.coloredlines.manual.game.board

import com.us.dsb.colorlines.game.board.{
  BallColor, BoardOrder, CellAddress, CellState, ColumnIndex, IndexOrigin, RowIndex}

// ?? TODO:  Revisit having companion object before class:
private[game] object Board {
  private[game] def empty: Board =
    Board(Vector.fill[CellState](BoardOrder * BoardOrder)(CellState.empty), Nil)
}
import Board.*


/**
 * Core state of board (just cells and on-deck balls; e.g.; no score, tap-UI selection).
 */
private[game] class Board(private val cellStates: Vector[CellState],
                          private val ondeckBalls: Iterable[BallColor]
                         ) {
  //println("* Board:   " + this)
  //print("")

  // internal/support methods:

  private def copy(cellStates: Vector[CellState] = cellStates,
                   ondeckBalls: Iterable[BallColor]  = ondeckBalls) =
    Board(cellStates, ondeckBalls)

  /** Computes row-major cell-array index from row and column numbers. */
  // ???? TODO: Use BoardOrder and IndexOrigin? Index.MinValue/.MaxValue?  colIndices.size?
  private def vectorIndex(address: CellAddress): Int =
    (address.row.raw.value - IndexOrigin)
        * BoardOrder
        + (address.column.raw.value - IndexOrigin)

  // on-deck balls:

  private[manual] def getOndeckBalls: Iterable[BallColor] = ondeckBalls

  private[game] def withOnDeckBalls(newBalls: Iterable[BallColor]): Board =
    copy(ondeckBalls = newBalls)

  // grid balls, getting:

  // ???? TODO:  Review Board and/or CellState re (previous?) excessive layering/wrapping:

  private[manual] def getCellStateAt(address: CellAddress): CellState =
    cellStates(vectorIndex(address))

  private[manual] def hasABallAt(address: CellAddress): Boolean =
    cellStates(vectorIndex(address)).asOption.isDefined

  private[manual] def isFull: Boolean = ! cellStates.exists(_.asOption.isEmpty)

  // grid balls, setting:

  private def withCellState(address: CellAddress,
                            newState: CellState): Board =
    copy(cellStates = cellStates.updated(vectorIndex(address), newState))

  private[game] def withBallAt(address: CellAddress,
                               ball: BallColor): Board =
    withCellState(address, CellState.withBallOfColor(ball))

  private[game] def withNoBallAt(address: CellAddress): Board =
    withCellState(address, CellState.empty)

  /** Makes compact single-line string like"<rgb------/---------/.../---------; (bgr) >". */
  override def toString: String = {
    import BallColorRenderingExtensions.*
    "<" ++
        RowIndex.values.map { row =>
          ColumnIndex.values.map { column =>
            val addr = CellAddress(row, column)
            getCellStateAt(addr).asOption.fold("-")(_.initial)
          }.mkString("")
        }.mkString("/") +
        " + " + getOndeckBalls.map(_.initial).mkString("(", ", ", ")") +
        ">"
  }

}
