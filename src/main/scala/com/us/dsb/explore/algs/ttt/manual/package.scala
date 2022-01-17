package com.us.dsb.explore.algs.ttt

import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.numeric.Interval.Closed
import eu.timepit.refined.predicates.all.Positive
import io.estatico.newtype.macros.newtype

package object manual {

  /** TTT row/column index integer; 1-based; top row, left column row are #1. */
  type Index = Int Refined Closed[1, 3]
  object Index extends RefinedTypeOps.Numeric[Index, Int]

  // ?? revisit use--in both table and UI selection model; separate?
  // ?? maybe move renderMultiline's rowIndices/etc. to around here

  import scala.language.implicitConversions
  @newtype case class RowIndex(value: Index)
  case class ColumnIndex(value: Int) extends AnyVal

}
