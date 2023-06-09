package com.us.dsb.explore.algs.coloredlines.manual.game.board

private[game] object LowerGameState {

  // ?? TODO:  Maybe rename:
  //   - "initial"?  but isn't state after initial-ball placement and before first move
  //   - "ground"? (from "ground state"--poor)
  private[game] def empty: LowerGameState =
    new LowerGameState(Board.empty, score = 0)
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

  private[game] def withBoard(board: Board): LowerGameState = copy(board = board)

  private[game] def withBoardWithBallAt(address: CellAddress, ball: BallColor): LowerGameState =
    copy(board = board.withBallAt(address, ball))
  private[game] def withBoardWithNoBallAt(address: CellAddress): LowerGameState =
    copy(board = board.withNoBallAt(address))

  // (running/total) score:

  private[game] def withAddedScore(increment: Int): LowerGameState =
    copy(score = this.score + increment)

  private[manual] def getScore: Int = score

  // renderings:

  /** Makes compact single-line string. */
  override def toString: String = "< " + board.toString + "; " + score + " pts" + ">"

}

