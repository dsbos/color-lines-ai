package com.us.dsb.colorlines.game.logic

import com.us.dsb.colorlines.game.board.{BoardReadView, CellAddress}

trait PathChecker {

  /**
   * Reports whether ball-movement path exists from given starting cell to
   * given target cell on given board.
   * @param board
   *   given board
   * @param fromBallCell
   *   address of starting cell (expected to contain ball (requirement undefined))
   * @param toEmptyCell
   *   address of target cell (expected to be different from starting cell
   *   (requirement undefined); must be empty, at least if not starting cell)
   * @return
   *   whether ball can be moved rom starting cell to target call
   */
  def pathExists(board: BoardReadView,
                 fromBallCell: CellAddress,
                 toEmptyCell: CellAddress): Boolean
}

