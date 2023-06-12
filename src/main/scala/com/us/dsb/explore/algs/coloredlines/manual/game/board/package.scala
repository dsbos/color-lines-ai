package com.us.dsb.explore.algs.coloredlines.manual.game

import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.numeric.Interval.Closed
import io.estatico.newtype.macros.newtype

package object board {

  /** Order (linear size) of board. */
  private[this] type BoardOrder = 9
  private[game] val BoardOrder: BoardOrder = valueOf[BoardOrder]

  // ???? TODO: Maybe move to package "lines" (with LineDetector) (level above
  //  raw board state).
  //  - Maybe gather multiple parameters together.
  //  - Also, maybe  have regular vs. reduced versions, with redirecting version
  //    with single source-level switch between them.
  /** Order (length) of lines. */
  private[this] type LineOrder = 5
  private[game] val LineOrder: LineOrder = valueOf[LineOrder]

  // ???? TODO:  Use this (tie to color enumeration) or remove.
  private[this] type ColorOrder = 7 // original: blue.dark, blue.light, brown, green, purple, red, yellow
  private[this] val ColorOrder: ColorOrder = valueOf[ColorOrder]

  /** Board row or column index integer; 1-based; top row, left column row are #1. */
  private[manual] type Index = Int Refined Closed[1, BoardOrder]
  private[manual] object Index extends RefinedTypeOps.Numeric[Index, Int]

  import scala.language.implicitConversions // suppress warning from @newtype

  // ?? what exactly does "private" on a newtype affect?
  @newtype private[board] case class RowIndex(value: Index)
  @newtype private[board] case class ColumnIndex(value: Index)

  //???? add utility methods like withColumnAdjustedBy(delta: Int)? (see GameUIState)

  // (unsafeFrom that should be okay since based on BoardOrder:)
  private[manual] val rowIndices: IndexedSeq[RowIndex] =
    (1 to BoardOrder).map(i => RowIndex(Index.unsafeFrom(i)))
  private[manual] val columnIndices: IndexedSeq[ColumnIndex] =
    (1 to BoardOrder).map(i => ColumnIndex(Index.unsafeFrom(i)))

  //?? factor out frequent row-and-column iteration pattern (set of cells, iterate, passing CellAddress


}
