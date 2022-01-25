package com.us.dsb.explore.algs.ttt.manual.game

import enumeratum.EnumEntry

/** Player identity. */
sealed trait Player extends EnumEntry
private[game] object Player {
  /** Player who goes first (per traditional rules/naming). */
  private[game] case object X extends Player
  private[game] case object O extends Player

  private[game] def getOtherPlayer(`given`: Player): Player = {
     `given` match {
       case X => O
       case O => X
     }
   }

}
