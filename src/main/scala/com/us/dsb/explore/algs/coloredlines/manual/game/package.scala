package com.us.dsb.explore.algs.coloredlines.manual

import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.numeric.Interval.Closed
import eu.timepit.refined.predicates.all.Positive
import io.estatico.newtype.macros.newtype

package object game {

  private[this] type BoardOrder = 4  //???? 9
  private[manual] val BoardOrder: BoardOrder = valueOf[BoardOrder]
  private[this] type LineOrder = 2  //???? 5
  private[this] val LineOrder: LineOrder = valueOf[LineOrder]
  private[this] type ColorOrder = 6  // blue.dark, blue.light, brown, green, purple, yellow
  private[this] val ColorOrder: ColorOrder = valueOf[ColorOrder]

  /** board row or column index integer; 1-based; top row, left column row are #1. */
  private[manual] type Index = Int Refined Closed[1, BoardOrder]
  private[manual] object Index extends RefinedTypeOps.Numeric[Index, Int]

  import scala.language.implicitConversions  // suppress warning from @newtype
  // ?? what exactly does "private" on a newtype affect?
  @newtype private[manual] case class RowIndex(value: Index)
  @newtype private[manual] case class ColumnIndex(value: Index)

  // (unsafeFrom that should be okay since based on BoardOrder:)
  private[manual] val rowIndices: IndexedSeq[RowIndex] =
    (1 to BoardOrder).map(i => RowIndex(Index.unsafeFrom(i)))
  private[manual] val columnIndices: IndexedSeq[ColumnIndex] =
    (1 to BoardOrder).map(i => ColumnIndex(Index.unsafeFrom(i)))

}
