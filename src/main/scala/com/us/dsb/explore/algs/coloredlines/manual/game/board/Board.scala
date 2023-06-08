package com.us.dsb.explore.algs.coloredlines.manual.game.board


private[game] object Board {

  /** Empty or ball of some color. */
  private[Board] case class CellBallState(ballState: Option[BallColor])
  private[Board] object CellBallState {
    private[Board] val empty: CellBallState = CellBallState(None)
  }

  private[game] def empty: Board =
    new Board(Vector.fill[CellBallState](BoardOrder * BoardOrder)(CellBallState.empty), Nil)
}
import Board._


/**
 * Core state of board (just cells and on-deck balls; e.g.; no score, tap-UI selection).
 */
private[game] class Board(private[this] val cellStates: Vector[CellBallState],
                          private[this] val onDeckBalls: Iterable[BallColor]
                         ) {
  //println("* Board:   " + this)
  //print("")

  // internal/support methods:

  private[this] def copy(cellStates: Vector[CellBallState] = cellStates,
                         onDeckBalls: Iterable[BallColor]  = onDeckBalls) =
    new Board(cellStates, onDeckBalls)

  /** Computes row-major cell-array index from row and column numbers. */
  private[this] def vectorIndex(address: CellAddress): Int =
    (address.row.value.value - 1) * BoardOrder + (address.column.value.value - 1)

  // on-deck balls:

  private[manual] def getOnDeckBalls: Iterable[BallColor] = onDeckBalls

  private[game] def withOnDeckBalls(newBalls: Iterable[BallColor]): Board =
    copy(onDeckBalls = newBalls)

  // grid balls, getting:

  private[manual] def getCellBallStateAt(address: CellAddress): CellBallState =
    cellStates(vectorIndex(address))

  private[manual] def getBallStateAt(address: CellAddress): Option[BallColor] =
    cellStates(vectorIndex(address)).ballState

  private[game] def hasABallAt(address: CellAddress): Boolean =
    cellStates(vectorIndex(address)).ballState.isDefined

  private[manual] def isFull: Boolean = ! cellStates.exists(_.ballState.isEmpty)

  // grid balls, setting:

  private def withCellBallState(address: CellAddress,
                                newState: CellBallState): Board =
    copy(cellStates = cellStates.updated(vectorIndex(address), newState))

  private[game] def withBallAt(address: CellAddress,
                               ball: BallColor): Board =
    withCellBallState(address, getCellBallStateAt(address).copy(ballState = Some(ball)))

  private[game] def withNoBallAt(address: CellAddress): Board =
    withCellBallState(address, getCellBallStateAt(address).copy(ballState = None))

  /** Makes compact single-line string like"<rgb------/---------/.../---------; (bgr) >". */
  override def toString: String = {
    "<" ++
        rowIndices.map { row =>
          columnIndices.map { column =>
            val addr = CellAddress(row, column)
            getCellBallStateAt(addr).ballState.fold("-")(_.initial)
          }.mkString("")
        }.mkString("/") +
        " + " + getOnDeckBalls.map(_.initial).mkString("(", ", ", ")") +
        ">"
  }

}
