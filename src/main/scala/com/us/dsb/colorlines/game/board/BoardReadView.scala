package com.us.dsb.colorlines.game.board

/** Read-only part of `Board` API (segregated for narrower dependency). */
trait BoardReadView {

  /** Gets (whole) cell state of specified cell. */
  def getCellStateAt(address: CellAddress): CellState

  def hasABallAt(address: CellAddress): Boolean =
    getCellStateAt(address).asOption.isDefined

  def isFull: Boolean =
    getBallCount == RowIndex.values.size * ColumnIndex.values.size

  // ???? TODO:  Revisit:  See if useful (beyond temp. use in path checker optimization logging).
  def getBallCount: Int

  def getOndeckBalls: Iterable[BallColor]

  def toCompactString: String
}

