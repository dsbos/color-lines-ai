package com.us.dsb.colorlines.crude

import com.us.dsb.colorlines.game.board.CellAddress

sealed trait PlayerMove derives CanEqual
object PlayerMove {
  case class  MoveBall(from: CellAddress, to: CellAddress) extends PlayerMove
  case object Pass                                         extends PlayerMove
  case object Quit                                         extends PlayerMove
}
