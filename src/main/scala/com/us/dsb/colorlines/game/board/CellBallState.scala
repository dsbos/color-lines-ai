package com.us.dsb.colorlines.game.board

import com.us.dsb.colorlines.game.board.BallColor

// ?? TODO:  Maybe re-investigate using union type BallColor | None.type.  (See 
//  my
//  https://users.scala-lang.org/t/how-to-select-union-type-branch-in-a-for-comprehension/9369/2).

// ???? TODO:  Review Board and/or CellBallState re excessive layering/wrapping.  

/** State of a cell--empty or having ball of some color. */
opaque type CellBallState = Option[BallColor]

object CellBallState {
  private val ballColorValues: Array[Some[BallColor]] =
    BallColor.values.map { color => Some(color) }

  /** Gets empty instance. */
  def empty: CellBallState = None

  /** Gets instance having given ball color. (Re-uses instances.) */
  def withBallOfColor(color: BallColor): CellBallState = ballColorValues(color.ordinal)

  extension (cellBallState: CellBallState)
    // ?????? TODO:  Revisit name:  "asOption":
    def ballState: Option[BallColor] = cellBallState

  // Note:  Opaque type can't have "derives CanEqual":
  given CanEqual[CellBallState, CellBallState] = CanEqual.derived
}
