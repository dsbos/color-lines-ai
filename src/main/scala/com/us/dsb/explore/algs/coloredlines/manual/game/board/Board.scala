package com.us.dsb.explore.algs.coloredlines.manual.game.board

import com.us.dsb.colorlines.game.board.{BallColor, BoardOrder}

// ?? TODO:  Revisit having companion object before class:
private[game] object Board {

  // ???? TODO:  Maybe collapse possibly excessive layer/wrapping here:
  /** State of a cell--empty or ball of some color. */
  private[Board] case class CellBallState(ballState: Option[BallColor])
  private[Board] object CellBallState {
    private[Board] val empty: CellBallState = CellBallState(None)
  }

  private[game] def empty: Board =
    Board(Vector.fill[CellBallState](BoardOrder * BoardOrder)(CellBallState.empty), Nil)
}
import Board.*


/**
 * Core state of board (just cells and on-deck balls; e.g.; no score, tap-UI selection).
 */
private[game] class Board(private val cellStates: Vector[CellBallState],
                          private val ondeckBalls: Iterable[BallColor]
                         ) {
  //println("* Board:   " + this)
  //print("")

  // internal/support methods:

  private def copy(cellStates: Vector[CellBallState] = cellStates,
                   ondeckBalls: Iterable[BallColor]  = ondeckBalls) =
    Board(cellStates, ondeckBalls)

  /** Computes row-major cell-array index from row and column numbers. */
  private def vectorIndex(address: CellAddress): Int =
    (address.row.raw.value - 1) * BoardOrder + (address.column.value.value - 1)

  // on-deck balls:

  private[manual] def getOndeckBalls: Iterable[BallColor] = ondeckBalls

  private[game] def withOnDeckBalls(newBalls: Iterable[BallColor]): Board =
    copy(ondeckBalls = newBalls)

  // grid balls, getting:

  // ???? TODO:  Maybe collapse possibly excessive layer/wrapping here:
  private[manual] def getCellBallStateAt(address: CellAddress): CellBallState =
    cellStates(vectorIndex(address))
  private[manual] def getBallStateAt(address: CellAddress): Option[BallColor] =
    getCellBallStateAt(address).ballState

  private[manual] def hasABallAt(address: CellAddress): Boolean =
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
    import BallColorRenderingExtensions.*
    "<" ++
        rowIndices.map { row =>
          columnIndices.map { column =>
            val addr = CellAddress(row, column)
            getCellBallStateAt(addr).ballState.fold("-")(_.initial)
          }.mkString("")
        }.mkString("/") +
        " + " + getOndeckBalls.map(_.initial).mkString("(", ", ", ")") +
        ">"
  }

}
