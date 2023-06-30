package com.us.dsb.colorlines.crude

import com.us.dsb.colorlines.game.board.BoardReadView

trait Player {
  def chooseMove(board: BoardReadView): PlayerMove
}
