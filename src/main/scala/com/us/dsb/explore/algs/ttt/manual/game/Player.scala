package com.us.dsb.explore.algs.ttt.manual.game

import enumeratum.EnumEntry

/** Player identity. */
sealed trait Player extends EnumEntry
object Player {
  case object O extends Player
  case object X extends Player
}
// ?? possibly put get-other-player logic here
