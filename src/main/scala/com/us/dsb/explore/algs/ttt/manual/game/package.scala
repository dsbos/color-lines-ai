package com.us.dsb.explore.algs.ttt.manual

import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.numeric.Interval.Closed
import eu.timepit.refined.predicates.all.Positive
import io.estatico.newtype.macros.newtype

package object game {

  private[manual] type Order = 3
  private[manual] val Order: Order = valueOf[Order]

  /** TTT row or column index integer; 1-based; top row, left column row are #1. */
  private[manual] type Index = Int Refined Closed[1, Order]
  private[manual] object Index extends RefinedTypeOps.Numeric[Index, Int]

  import scala.language.implicitConversions  // suppress warning from @newtype
  // ?? what exactly does "private" on a newtype affect?
  @newtype case class RowIndex(value: Index)
  @newtype case class ColumnIndex(value: Index)

  // (unsafeFrom that should be okay since based on Order:)
  private[manual] val rowIndices: IndexedSeq[RowIndex] =
    (1 to Order).map(i => RowIndex(Index.unsafeFrom(i)))
  private[manual] val columnIndices: IndexedSeq[ColumnIndex] =
    (1 to Order).map(i => ColumnIndex(Index.unsafeFrom(i)))

}
