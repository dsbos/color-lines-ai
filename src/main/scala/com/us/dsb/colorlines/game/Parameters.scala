package com.us.dsb.colorlines.game

// ???? TODO: Maybe have regular vs. reduced versions, with redirecting version
//    having single source-level switch between them (or runtime switch?).

object Parameters {

  /** Order (linear size) of board, as type for refined type [[Index]]. */
  type BoardOrder = 9

  /** Order (linear size) of board, as regular value. */
  val BoardOrder: BoardOrder = valueOf[BoardOrder]

  private[game] val InitialBallCount: Int = 5

  private[game] val OnDeckBallCount: Int = 3

  /** Order (minimum length) of scorable lines. */
  private[game] val LineOrder: Int = 5

  // ????? TODO:  Revisit name.
  private[game] def computeReapingScore(numberOfBallsRemoved: Int): Int = {
    // Original game scoring was score = <number of balls removed> * 4 - 10,
    //  which seems to be from 2 points per ball in 5-ball line, but 4 each for
    //  any balls in line beyond 5.
    2 * LineOrder + 4 * (numberOfBallsRemoved - LineOrder)
  }

}
