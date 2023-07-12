package com.us.dsb.colorlines.crude

import com.us.dsb.colorlines.game.board.BoardReadView

// ?????? TODO:  Determine how to let user get feedback on move, especially
//   attempted ball-move moves; maybe:
// - make player call back to get validated-move object to return
// - let player ask about move before returning (but player could return other, bad move)
// - let player call to drive moving--but inverts intended control (maybe only
//   partially:  manager calls player, player tries to move, uon successful
//   move, player returns--but complicated and less controlled)
// - pass result/status of previous move to chooseMove (but probably inconvenient
//   state management for user)

trait Player {
  def chooseMove(board: BoardReadView): PlayerMove
}
