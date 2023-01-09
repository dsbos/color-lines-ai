package com.us.dsb.explore.algs.coloredlines.manual.game.board

private[game] object BoardPlus {

  private[game] def empty: BoardPlus = new BoardPlus(BoardState.empty,0, None)
}

//???? move tap-UI selection state out of this low-level game state
/**
 * CURRENTLY:  Core board state (now wrapped), not score yet, tap-UI selection state
 */
private[game] class BoardPlus(private[manual] val boardState: BoardState,
                              private[this] val score: Int,
                              //???? move to (low-level) tap-UI state:
                              private[this] val selectionAddress: Option[CellAddress]
                             ) {
  println("??? BoardPlus : " + this)
  //print("")

  // internal/support methods:

  private[this] def copy(boardState: BoardState                = boardState,
                         score: Int                            = score,
                         selectionAddress: Option[CellAddress] = selectionAddress) =
    new BoardPlus(boardState, score, selectionAddress)

  // grid balls:

  private[game] def isFull: Boolean = boardState.isFull

  private[manual/*game*/] def getBallStateAt(address: CellAddress): Option[BallKind] =
    boardState.getBallStateAt(address)

  private[game] def hasABallAt(address: CellAddress): Boolean =
    boardState.hasABallAt(address)

  private[game] def withBallAt(address: CellAddress, ball: BallKind): BoardPlus =
    copy(boardState = boardState.withBallAt(address, ball))

  private[game] def withNoBallAt(address: CellAddress): BoardPlus =
    copy(boardState = boardState.withNoBallAt(address))


  private[game] def withCellSelected(address: CellAddress): BoardPlus =
    copy(selectionAddress = Some(address))

  private[game] def withNoSelection: BoardPlus =
    copy(selectionAddress = None)

  //???? move out?
  private[manual] def getCellBallStateChar(ballState: Option[BallKind], isSelected: Boolean): String = {
    ballState match {
      case Some(ball) => ball.getColoredCharSeq(isSelected)
      case None       => if (! isSelected) "-" else "@"
    }
  }

  // (lower-level) board state

  private[game] def withBoardState(boardState: BoardState): BoardPlus =
    copy(boardState = boardState)

  // (running) score:

  private[game] def withAddedScore(increment: Int): BoardPlus =
    copy(score = this.score + increment)

  private[manual] def getScore: Int = score

  // top-UI selection:

  private[game] def hasAnyCellSelected: Boolean = selectionAddress.isDefined
  private[game] def getSelectionCoordinates: Option[CellAddress] = selectionAddress
  private[manual] def isSelectedAt(address: CellAddress): Boolean =
    selectionAddress.fold(false)(_ == address)

  private[game] def hasABallSelected: Boolean = selectionAddress.fold(false)(hasABallAt)

  // renderings:

  /** Makes compact single-line string. */
  override def toString: String = "< " + boardState.toString + "; " + score + " pts" + ">"

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
        "" + getCellBallStateChar(getBallStateAt(addr), isSelectedAt(addr)) + " "
      }.mkString(cellSeparator)  // make each row line
    }.mkString(rowSeparator)     // make whole-board multi-line string
  }

  private[this] def renderCompactMultiline: String = {
    rowIndices.map { row =>
      columnIndices.map { column =>
        val addr = CellAddress(row, column)
        getCellBallStateChar(getBallStateAt(addr), isSelectedAt(addr))
      }.mkString("|")  // make each row line
    }.mkString("\n")
  }

}

