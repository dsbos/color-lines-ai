package com.us.dsb.explore.algs.ttt.manual.game

import enumeratum.EnumEntry

/** Player identity. */
sealed class Player(val otherPlayer: Player) extends EnumEntry
private[game] object Player {
  /** Player who goes first (per traditional rules/naming). */
  private[game] case object X extends Player(O)
  private[game] case object O extends Player(X)
}
