package com.us.dsb.explore.algs.ttt

import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.numeric.Interval.Closed
import eu.timepit.refined.predicates.all.Positive
import io.estatico.newtype.macros.newtype

package object manual {

  // ?? should these be in Board?  should these be with Board in subpackage "board"?

  // ?? can one of these be derived from the other?
  val Order = 3
  type Order = 3

  /** TTT row or column index integer; 1-based; top row, left column row are #1. */
  type Index = Int Refined Closed[1, Order]
  object Index extends RefinedTypeOps.Numeric[Index, Int]

  // ?? revisit use--in both table and UI selection model; separate?

  import scala.language.implicitConversions  // suppress warning from  @newtype
  @newtype case class RowIndex(value: Index)
  @newtype case class ColumnIndex(value: Index)

  // (unsafeFrom that should be okay since based on Order:)
  val rowIndices: IndexedSeq[RowIndex] =
    (1 to Order).map(i => RowIndex(Index.unsafeFrom(i)))
  val columnIndices: IndexedSeq[ColumnIndex] =
    (1 to Order).map(i => ColumnIndex(Index.unsafeFrom(i)))

}
