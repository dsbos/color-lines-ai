package com.us.dsb.explore.algs.ttt.manual.game

import enumeratum.EnumEntry

/** Player identity. */
sealed trait Player extends EnumEntry
private/*??*/ object Player {
  private[game]/*??*/ case object O extends Player
  private[game]/*??*/ case object X extends Player
}
// ?? possibly put get-other-player logic here
