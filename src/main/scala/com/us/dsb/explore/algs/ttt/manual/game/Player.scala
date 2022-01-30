package com.us.dsb.explore.algs.ttt.manual.game

import enumeratum.EnumEntry

/** Player identity. */
sealed trait Player extends EnumEntry {
  def opponent: Player
}
// (Object Player, X, and Y visible by test in [ui].)
object Player {

  /** Player who goes first (per traditional rules/naming). */
  case object X extends Player {
    override def opponent: O.type = O
  }

  case object O extends Player {
    def opponent: X.type = X
  }

}
