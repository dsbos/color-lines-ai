package com.us.dsb.colorlines.game

import com.us.dsb.colorlines.game.board.{BallColor, Board, CellAddress}

object GameState {

  // ?? TODO:  Maybe rename:
  //   - "initial"?  but isn't state after initial-ball placement and before first move
  //   - "ground"? (from "ground state"--poor)
  def empty: GameState = GameState(Board.empty, score = 0)
}

/**
 * (Lower-level): game state:  board and score.  (No more tap-UI selection state.)
 */
class GameState(val board: Board,
                val score: Int) {
  println("* GameState  : " + this)

  // internal/support methods:

  private def copy(board: Board = board,
                   score: Int   = score): GameState =
    GameState(board, score)

  // board state:

  /*private[game]*/ def withBoard(board: Board): GameState = copy(board = board)

  /*private[game]*/ def withBoardWithBallAt(address: CellAddress, ball: BallColor): GameState =
    copy(board = board.withBallAt(address, ball))
  /*private[game]*/ def withBoardWithNoBallAt(address: CellAddress): GameState =
    copy(board = board.withNoBallAt(address))

  // (running/total) score:

  /*private[game]*/ def withAddedScore(increment: Int): GameState =
    copy(score = this.score + increment)

  def getScore: Int = score

  // renderings:

  /** Makes compact single-line string. */
  def toCompactString: String = "< " + board.toCompactString + "; " + score + " pts" + ">"

  /** Sames as `toCompactString`. */
  override def toString: String = toCompactString
}

