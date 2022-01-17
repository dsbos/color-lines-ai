package com.us.dsb.explore.algs.ttt

import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.numeric.Interval.Closed
import eu.timepit.refined.predicates.all.Positive

package object manual {

  type Index = Int Refined (Closed[1, 3])
  object Index extends RefinedTypeOps.Numeric[Index, Int]

  // ?? clean implementation (probably refined Int, maybe enum.
  // ?? revisit use--in table and UI selection model; separate?
  //???????type Index = Int
  // 1: top row/leftmost column

  type RowIndex = Index
  val RowIndex = Index

  // Note:  Can wrap Index here:
  case class ColumnIndex(value: Int) extends AnyVal

}
