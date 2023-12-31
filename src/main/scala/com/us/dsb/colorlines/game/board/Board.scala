package com.us.dsb.colorlines.game.board

import com.us.dsb.colorlines.game.Parameters.BoardOrder
import com.us.dsb.colorlines.game.board.{CellState, ColumnIndex, Index, RowIndex}

// ?? TODO:  Revisit having companion object before class:
private[game] object Board {
  private[game] def empty: Board =
    Board(Vector.fill[CellState](BoardOrder * BoardOrder)(CellState.empty), Nil)
}

/**
 * Core state of board (just cells and on-deck balls; e.g.; no score, tap-UI selection).
 */
private[game] class Board(private val cellStates: Vector[CellState],
                          private val ondeckBalls: Iterable[BallColor]
                         )
    extends BoardReadView {

  // Internal/support methods (most):

  private def copy(cellStates: Vector[CellState] = cellStates,
                   ondeckBalls: Iterable[BallColor] = ondeckBalls) =
    Board(cellStates, ondeckBalls)

  /** Computes row-major cell-array index from row and column numbers. */
  // ???? TODO: Use BoardOrder and IndexOrigin? Index.MinValue/.MaxValue?  colIndices.size?
  private def vectorIndex(address: CellAddress): Int =
    (address.row.raw.value - Index.Origin)
        * BoardOrder
        + (address.column.raw.value - Index.Origin)

  // Read-only API (BoardReadView) methods:

  override def getCellStateAt(address: CellAddress): CellState =
    cellStates(vectorIndex(address))

  // ?? TODO:  Maybe optimize:  Store ball/vacancy count to avoid scan,
  //   especially for isFull.
  override def isFull: Boolean = ! cellStates.exists(_.asOption.isEmpty)

  override def getBallCount: Int = cellStates.count(_.asOption.isDefined)

  override def getOndeckBalls: Iterable[BallColor] = ondeckBalls

  // Mutation methods:

  private def withCellState(address: CellAddress,
                            newState: CellState): Board =
    copy(cellStates = cellStates.updated(vectorIndex(address), newState))

  private[game] def withBallAt(address: CellAddress,
                               ball: BallColor): Board =
    withCellState(address, CellState.withBallOfColor(ball))

  private[game] def withNoBallAt(address: CellAddress): Board =
    withCellState(address, CellState.empty)

  private[game] def withOnDeckBalls(newBalls: Iterable[BallColor]): Board =
    copy(ondeckBalls = newBalls)

  // Miscellaneous:

  /** Makes compact single-line string like"<RGB------/---------/.../--------- + (B, G, R)>". */
  override def toCompactString: String = {
    import BallColorRenderingExtensions.*
    "<"
        + RowIndex.values.map { row =>
            ColumnIndex.values.map { column =>
              val addr = CellAddress(row, column)
              getCellStateAt(addr).asOption.fold("-")(_.initial)
            }.mkString("")
          }.mkString("/")
        + " + "
        + getOndeckBalls.map(_.initial).mkString("(", ", ", ")")
        + ">"
  }

  /** Sames as `toCompactString`. */
  override def toString: String = toCompactString

}
