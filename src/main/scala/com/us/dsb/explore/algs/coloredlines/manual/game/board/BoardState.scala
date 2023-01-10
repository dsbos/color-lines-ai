package com.us.dsb.explore.algs.coloredlines.manual.game.board


private[game] object Board {

  /** Empty or ball of some color. */
  private[Board] case class CellBallState(ballState: Option[BallKind])
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
                               private[this] val onDeck: Iterable[BallKind]
                              ) {
  //println("??? Board:   " + this)
  //print("")

  // internal/support methods:

  private[this] def copy(cellStates: Vector[CellBallState] = cellStates,
                         onDeck: Iterable[BallKind]        = onDeck) =
    new Board(cellStates, onDeck)

  /** Computes row-major cell-array index from row and column numbers. */
  private[this] def vectorIndex(address: CellAddress): Int =
    (address.row.value.value - 1) * BoardOrder + (address.column.value.value - 1)

  // on-deck balls:

  private[manual] def getOnDeckBalls: Iterable[BallKind] = onDeck

  private[game] def withOnDeckBalls(newBalls: Iterable[BallKind]): Board =
    copy(onDeck = newBalls)

  // grid balls, getting:

  private[manual] def getCellBallStateAt(address: CellAddress): CellBallState =
    cellStates(vectorIndex(address))

  private[manual] def getBallStateAt(address: CellAddress): Option[BallKind] =
    cellStates(vectorIndex(address)).ballState

  private[game] def hasABallAt(address: CellAddress): Boolean =
    cellStates(vectorIndex(address)).ballState.isDefined

  private[game] def isFull: Boolean = ! cellStates.exists(_.ballState.isEmpty)

  // grid balls, setting:

  private def withCellBallState(address: CellAddress,
                                newState: CellBallState): Board =
    copy(cellStates = cellStates.updated(vectorIndex(address), newState))

  private[game] def withBallAt(address: CellAddress,
                               ball: BallKind): Board =
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
