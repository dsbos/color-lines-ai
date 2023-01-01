package com.us.dsb.explore.algs.coloredlines.manual.game

private[manual] object Board {

  /** Empty or ball of some color. */
  private[game] case class CellState(ballState: Option[BallKind])
  private[game] object CellState {
    private[game] val empty: CellState = CellState(None)
  }

  private[manual] case class CellAddress(row: RowIndex, column: ColumnIndex)

  private[game] def empty: Board =
    new Board(Vector.fill[CellState](BoardOrder * BoardOrder)(CellState.empty), Nil, None)
}

import Board._

/**
 * State of board (just cells; not other game state (e.g., score).)
 */
private[manual] class Board(private[this] val cellStates: Vector[CellState],
                            private[this] val onDeck: Iterable[BallKind],
                           //???? move to game (low-level UI) state:
                            private[this] val selectionAddress: Option[CellAddress]
                           ) {

  /** Computes row-major cell-array index from row and column numbers. */
  private[this] def vectorIndex(address: CellAddress): Int =
    (address.row.value.value - 1) * BoardOrder + (address.column.value.value - 1)


  private[manual] def getCellStateAt(address: CellAddress): CellState = {
    cellStates(vectorIndex(address))
  }

  private[game] def getOnDeckBalls: Iterable[BallKind] = onDeck

  private[game] def withOnDeckBalls(newBalls: Iterable[BallKind]): Board =
    new Board(cellStates, newBalls, selectionAddress)

  private[game] def isFull: Boolean = ! cellStates.exists(_.ballState.isEmpty)

  private[game] def getBallStateAt(address: CellAddress): Option[BallKind] = {
    cellStates(vectorIndex(address)).ballState
  }
  private[game] def hasABallAt(address: CellAddress): Boolean = {
    cellStates(vectorIndex(address)).ballState.isDefined
  }
  private[manual] def isSelectedAt(xxaddress: CellAddress): Boolean = {
    //???????? simple equality
    selectionAddress.fold(false)(current => current.row == xxaddress.row && current.column == xxaddress.column)
  }
  private[game] def hasABallSelected: Boolean =
    selectionAddress.fold(false)(coords => hasABallAt(coords))

  private[game] def hasAnyCellSelected: Boolean = selectionAddress.isDefined

  private[game] def getSelectionCoordinates: Option[CellAddress] =
    selectionAddress

  private def withCellState(address: CellAddress,
                            newState: CellState): Board =
    new Board(cellStates.updated(vectorIndex(address), newState), onDeck, selectionAddress)

  private[game] def withCellHavingBall(address: CellAddress,
                                       ball: BallKind): Board =
    withCellState(address, getCellStateAt(address).copy(ballState = Some(ball)))

  private[game] def withCellSelected(address: CellAddress): Board =
    new Board(cellStates, onDeck, Some(address))

  private[game] def withNoSelection: Board =
    new Board(cellStates, onDeck, None)

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

  private[manual] def getStateChar(state: CellState, isSelected: Boolean): String = {  //???? move out
    state.ballState match {
      case Some(ball) => ball.getColoredCharSeq(isSelected)
      case None => if (! isSelected) "-" else "@"
    }
  }

  /** Makes compact single-line string like Xx"<X-O/-X-/O-X>". */
  override def toString: String = {
    rowIndices.map { row =>
      columnIndices.map { column =>
        val addr = CellAddress(row, column)
        getStateChar(getCellStateAt(addr), isSelectedAt(addr))
      }.mkString("")
    }.mkString("<", "/", ">")
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
        "" + getStateChar(getCellStateAt(addr), isSelectedAt(addr)) + " "
      }.mkString(cellSeparator)  // make each row line
    }.mkString(rowSeparator)     // make whole-board multi-line string
  }

  private[this] def renderCompactMultiline: String = {
    rowIndices.map { row =>
      columnIndices.map { column =>
        val addr = CellAddress(row, column)
        getStateChar(getCellStateAt(addr), isSelectedAt(addr))
      }.mkString("|")  // make each row line
    }.mkString("\n")
  }

}

