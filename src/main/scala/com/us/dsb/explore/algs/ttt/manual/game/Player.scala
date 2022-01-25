package com.us.dsb.explore.algs.ttt.manual.game

import enumeratum.EnumEntry

/** Player identity. */
sealed trait Player extends EnumEntry {
  def opponent: Player
}
private[game] object Player {

  /** Player who goes first (per traditional rules/naming). */
  private[game] case object X extends Player {
    override def opponent: O.type = O
  }

  private[game] case object O extends Player {
    def opponent: X.type = X
  }

}
