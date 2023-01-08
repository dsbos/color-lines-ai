package com.us.dsb.explore.algs.coloredlines.manual.game.board

private[game] object Board {

  /** Empty or ball of some color. */
  private[Board] case class CellBallState(ballState: Option[BallKind])
  private[Board] object CellBallState {
    private[Board] val empty: CellBallState = CellBallState(None)
  }

  private[game] def empty: Board =
    new Board(Vector.fill[CellBallState](BoardOrder * BoardOrder)(CellBallState.empty), Nil, None)
}

import com.us.dsb.explore.algs.coloredlines.manual.game.board.Board._
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{BoardOrder, columnIndices, rowIndices}

//???? move tap-UI selection state out of board
/**
 * State of board (just cells; not other game state (e.g., score).)
 */
private[game] class Board(private[this] val cellStates: Vector[CellBallState],  //????? grid?
                            private[this] val onDeck: Iterable[BallKind],
                            //???? move to game (low-level tap-UI) state:
                            private[this] val selectionAddress: Option[CellAddress]
                           ) {
  //??? println("??? Board: " + this)
  //??? print("")

  // internal/support methods:

  private[this] def copy(cellStates: Vector[CellBallState] = cellStates,
                         onDeck: Iterable[BallKind] = onDeck,
                         selectionAddress: Option[CellAddress] = selectionAddress) =
    new Board(cellStates, onDeck, selectionAddress)

  /** Computes row-major cell-array index from row and column numbers. */
  private[this] def vectorIndex(address: CellAddress): Int =
    (address.row.value.value - 1) * BoardOrder + (address.column.value.value - 1)

  // on-deck balls:
  private[game] def getOnDeckBalls: Iterable[BallKind] = onDeck

  private[game] def withOnDeckBalls(newBalls: Iterable[BallKind]): Board =
    copy(onDeck = newBalls)

  // grid balls:

  private[manual] def getCellBallStateAt(address: CellAddress): CellBallState = {
    cellStates(vectorIndex(address))
  }

  private[game] def isFull: Boolean = ! cellStates.exists(_.ballState.isEmpty)

  private[game] def getBallStateAt(address: CellAddress): Option[BallKind] = {
    cellStates(vectorIndex(address)).ballState
  }
  private[game] def hasABallAt(address: CellAddress): Boolean =
    cellStates(vectorIndex(address)).ballState.isDefined


  //

  private def withCellBallState(address: CellAddress,
                                newState: CellBallState): Board =
    copy(cellStates = cellStates.updated(vectorIndex(address), newState))

  private[game] def withBallAt(address: CellAddress,
                               ball: BallKind): Board =
    withCellBallState(address, getCellBallStateAt(address).copy(ballState = Some(ball)))

  private[game] def withNoBallAt(address: CellAddress): Board =
    withCellBallState(address, getCellBallStateAt(address).copy(ballState = None))

  private[game] def withCellSelected(address: CellAddress): Board =
    copy(selectionAddress = Some(address))

  private[game] def withNoSelection: Board =
    copy(selectionAddress = None)

  private[manual] def getCellBallStateChar(state: CellBallState, isSelected: Boolean): String = {  //???? move out
    state.ballState match {
      case Some(ball) => ball.getColoredCharSeq(isSelected)
      case None       => if (! isSelected) "-" else "@"
    }
  }

  // top-UI selection:

  private[game] def hasAnyCellSelected: Boolean = selectionAddress.isDefined
  private[game] def getSelectionCoordinates: Option[CellAddress] = selectionAddress
  private[manual] def isSelectedAt(address: CellAddress): Boolean =
    selectionAddress.fold(false)(_ == address)

  private[game] def hasABallSelected: Boolean = selectionAddress.fold(false)(hasABallAt)

  // renderings:

  /** Makes compact single-line string like Xx"<X-O/-X-/O-X>". */
  override def toString: String = {
    "<" ++
    rowIndices.map { row =>
      columnIndices.map { column =>
        val addr = CellAddress(row, column)
        getCellBallStateAt(addr).ballState.fold("-")(_.initial)
      }.mkString("")
    }.mkString("/") ++
        " + " ++ getOnDeckBalls.map(_.initial).mkString("(", ", ", ")") ++
        ">"
  }

  private[this] def renderMultiline: String = {
    val cellWidth = " X ".length
    val cellSeparator = "|"
    val wholeWidth =
      columnIndices.length * cellWidth +
          (columnIndices.length - 1) * cellSeparator.length
    val rowSeparator = "\n" + ("-" * wholeWidth) + "\n"

    rowIndices.map { row =>
      columnIndices.map { column =>
        val addr = CellAddress(row, column)
        "" + getCellBallStateChar(getCellBallStateAt(addr), isSelectedAt(addr)) + " "
      }.mkString(cellSeparator)  // make each row line
    }.mkString(rowSeparator)     // make whole-board multi-line string
  }

  private[this] def renderCompactMultiline: String = {
    rowIndices.map { row =>
      columnIndices.map { column =>
        val addr = CellAddress(row, column)
        getCellBallStateChar(getCellBallStateAt(addr), isSelectedAt(addr))
      }.mkString("|")  // make each row line
    }.mkString("\n")
  }

}

