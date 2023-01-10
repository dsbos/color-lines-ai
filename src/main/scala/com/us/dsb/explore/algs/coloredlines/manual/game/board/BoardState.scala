package com.us.dsb.explore.algs.coloredlines.manual.game.board


private[game] object BoardState {

  /** Empty or ball of some color. */
  private[BoardState] case class CellBallState(ballState: Option[BallKind])
  private[BoardState] object CellBallState {
    private[BoardState] val empty: CellBallState = CellBallState(None)
  }

  private[game] def empty: BoardState =
    new BoardState(Vector.fill[CellBallState](BoardOrder * BoardOrder)(CellBallState.empty), Nil)
}
import BoardState._


/**
 * Core state of board (just cells and on-deck balls; e.g.; no score, tap-UI selection).
 */
private[game] class BoardState(private[this] val cellStates: Vector[CellBallState],
                               private[this] val onDeck: Iterable[BallKind]
                              ) {
  //println("??? BoardState:   " + this)
  //print("")

  // internal/support methods:

  private[this] def copy(cellStates: Vector[CellBallState] = cellStates,
                         onDeck: Iterable[BallKind]        = onDeck) =
    new BoardState(cellStates, onDeck)

  /** Computes row-major cell-array index from row and column numbers. */
  private[this] def vectorIndex(address: CellAddress): Int =
    (address.row.value.value - 1) * BoardOrder + (address.column.value.value - 1)

  // on-deck balls:

  private[manual] def getOnDeckBalls: Iterable[BallKind] = onDeck

  private[game] def withOnDeckBalls(newBalls: Iterable[BallKind]): BoardState =
    copy(onDeck = newBalls)

  // grid balls, getting:

  private[manual] def getCellBallStateAt(address: CellAddress): CellBallState =
    cellStates(vectorIndex(address))

  private[game] def getBallStateAt(address: CellAddress): Option[BallKind] =
    cellStates(vectorIndex(address)).ballState

  private[game] def hasABallAt(address: CellAddress): Boolean =
    cellStates(vectorIndex(address)).ballState.isDefined

  private[game] def isFull: Boolean = ! cellStates.exists(_.ballState.isEmpty)

  // grid balls, setting:

  private def withCellBallState(address: CellAddress,
                                newState: CellBallState): BoardState =
    copy(cellStates = cellStates.updated(vectorIndex(address), newState))

  private[game] def withBallAt(address: CellAddress,
                               ball: BallKind): BoardState =
    withCellBallState(address, getCellBallStateAt(address).copy(ballState = Some(ball)))

  private[game] def withNoBallAt(address: CellAddress): BoardState =
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
