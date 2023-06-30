package com.us.dsb.colorlines.crude

import com.us.dsb.colorlines.game.board.BoardReadView

/**
 * Runs PlayManagerNGamesWStats, playing by just passing (not moving balls).
 */
object PlayJustPassingNGamesWStats extends App {
  PlayManagerNGamesWStats.run(
    new Player {
      def chooseMove(board: BoardReadView): PlayerMove = PlayerMove.Pass
    }
    )
}
