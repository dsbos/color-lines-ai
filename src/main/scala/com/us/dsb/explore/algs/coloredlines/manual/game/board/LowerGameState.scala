package com.us.dsb.explore.algs.coloredlines.manual.game.board

private[game] object LowerGameState {

  private[game] def empty: LowerGameState = new LowerGameState(Board.empty, 0)
}

/**
 * (Lower-level): game state:  board and score.  (No more tap-UI selection state.)
 */
private[game] class LowerGameState(private[manual] val board: Board,
                                   private[this] val score: Int
                                 ) {
  println("* LowerGameState  : " + this)
  //print("")

  // internal/support methods:

  private[this] def copy(board: Board = board,
                         score: Int   = score) =
    new LowerGameState(board, score)

  // board state:

  private[game] def withBoard(board: Board): LowerGameState =
    copy(board = board)

  // (running/total) score:

  private[game] def withAddedScore(increment: Int): LowerGameState =
    copy(score = this.score + increment)

  private[manual] def getScore: Int = score



  // grid balls:

  //?????? remove these mutation-support methods; have callers use withBoard

  private[game] def withBallAt(address: CellAddress, ball: BallKind): LowerGameState =
    copy(board = board.withBallAt(address, ball))

  private[game] def withNoBallAt(address: CellAddress): LowerGameState =
    copy(board = board.withNoBallAt(address))


  //???? move up?  (up to tap-UI state with selection? but note that getColoredCharSeq know a bit about selection
  private[manual] def getCellBallStateChar(ballState: Option[BallKind], isSelected: Boolean): String = {
    ballState match {
      case Some(ball) => ball.getColoredCharSeq(isSelected)
      case None       => if (! isSelected) "-" else "@"
    }
  }

  // renderings:

  /** Makes compact single-line string. */
  override def toString: String = "< " + board.toString + "; " + score + " pts" + ">"

  private[this] def renderMultiline(selectionAddress: Option[CellAddress]): String = {
    val cellWidth = " X ".length
    val cellSeparator = "|"
    val wholeWidth =
      columnIndices.length * cellWidth +
          (columnIndices.length - 1) * cellSeparator.length
    val rowSeparator = "\n" + ("-" * wholeWidth) + "\n"

    rowIndices.map { row =>
      columnIndices.map { column =>
        val addr = CellAddress(row, column)
        val isSelected = selectionAddress.fold(false)(_ == addr)
        "" + getCellBallStateChar(board.getBallStateAt(addr), isSelected) + " "
      }.mkString(cellSeparator)  // make each row line
    }.mkString(rowSeparator)     // make whole-board multi-line string
  }

  private[this] def renderCompactMultiline(selectionAddress: Option[CellAddress]): String = {
    rowIndices.map { row =>
      columnIndices.map { column =>
        val addr = CellAddress(row, column)
        val isSelected = selectionAddress.fold(false)(_ == addr)
        getCellBallStateChar(board.getBallStateAt(addr), isSelected)
      }.mkString("|")  // make each row line
    }.mkString("\n")
  }

}

