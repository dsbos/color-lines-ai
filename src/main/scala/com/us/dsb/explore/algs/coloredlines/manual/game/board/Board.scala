package com.us.dsb.explore.algs.coloredlines.manual.game.board

import com.us.dsb.colorlines.game.board.{
  BallColor, BoardOrder, CellAddress, CellState, ColumnIndex, IndexOrigin, RowIndex}

/** Read-only part of `Board` API (segregated for narrower dependency). */
trait BoardReadView {
  def getCellStateAt(address: CellAddress): CellState

  def hasABallAt(address: CellAddress): Boolean

  def isFull: Boolean

  def getOndeckBalls: Iterable[BallColor]
}

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
                   ondeckBalls: Iterable[BallColor]  = ondeckBalls) =
    Board(cellStates, ondeckBalls)

  /** Computes row-major cell-array index from row and column numbers. */
  // ???? TODO: Use BoardOrder and IndexOrigin? Index.MinValue/.MaxValue?  colIndices.size?
  private def vectorIndex(address: CellAddress): Int =
    (address.row.raw.value - IndexOrigin)
        * BoardOrder
        + (address.column.raw.value - IndexOrigin)

  // Read-only API (BoardReadView) methods:

  override def getCellStateAt(address: CellAddress): CellState =
    cellStates(vectorIndex(address))

  override def hasABallAt(address: CellAddress): Boolean =
    getCellStateAt(address).asOption.isDefined

  override def isFull: Boolean = !cellStates.exists(_.asOption.isEmpty)

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

  // ?????? TODO:  Maybe move to toDebugString/toLogString/toCompactString, adding
  //   to BoardReadView (for future game-playing logic).
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
