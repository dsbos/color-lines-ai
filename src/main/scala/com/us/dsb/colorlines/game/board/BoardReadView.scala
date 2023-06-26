package com.us.dsb.colorlines.game.board

/** Read-only part of `Board` API (segregated for narrower dependency). */
trait BoardReadView {
  def getCellStateAt(address: CellAddress): CellState

  def hasABallAt(address: CellAddress): Boolean

  def isFull: Boolean

  def getOndeckBalls: Iterable[BallColor]

  def toCompactString: String
}

