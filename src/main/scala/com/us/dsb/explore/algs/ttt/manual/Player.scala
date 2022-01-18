package com.us.dsb.explore.algs.ttt.manual

import enumeratum.EnumEntry

sealed trait Player extends EnumEntry
object Player {
  case object O extends Player
  case object X extends Player
}
// ?? possibly put get-other-player logic here
