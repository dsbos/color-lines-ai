package com.us.dsb.colorlines.game.board

import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.numeric.Interval.Closed

// ???? TODO: Probably gather together multiple parameters (board order, line
//   order, numbers of initial and added balls, scoring).
//   Also, maybe have regular vs. reduced versions, with redirecting version
//    having single source-level switch between them (or runtime switch?).

// ???? TODO:  Maybe move BoardOrder to Index.Something, parallel to Index.Origin.
//   Maybe move multiple top-level declarations into an indexing object or
//   package, maybe making some private.)

/** Order (linear size) of board, as type for refined type [[Index]]. */
type BoardOrder = 9

/** Order (linear size) of board, as regular value. */
val BoardOrder: BoardOrder = valueOf[BoardOrder]

/** Board row or column index integer; 1-based; top row, left column are #1. */
// (Upper bound BoardOrder is lower bound 1 plus BoardOrder minus 1 re delta.)
type Index = Int Refined Closed[Index.Origin, BoardOrder]

// ?? TODO 2->3 . refined:  Review refined_3 way to do following:
// ?? TODO 2-?3 . refined:  Investigate Iron refined-types library:
import scala.language.adhocExtensions  // re extending non-"open" Numeric (_3:1.7.2):
object Index extends RefinedTypeOps.Numeric[Index, Int] {

  /** Index origin for row/column indexes, as type for refined type [[Index]]. */
  type Origin = 1

  /** Index origin for row/column indexes, as regular value. */
  val Origin: Origin = valueOf[Origin]

  val values: IndexedSeq[Index] = (Index.MinValue.value to Index.MaxValue.value).map(Index.unsafeFrom(_))
  // ???? TODO:  Review whether to add cardinality member (returning BoardOrder),
  //   so code using Index.value uses another Index.Xyz instead of separate
  //   BoardOrder (any maybe val Board order can be hidden (by moving top-level
  //   things into an Indexing object)).
}

/** Row index value. */
opaque type RowIndex = Index

object RowIndex {
  def apply(raw: Index): RowIndex = raw

  extension (rowIndex: RowIndex)
    def raw: Index = rowIndex: Index

  val values: IndexedSeq[RowIndex] = Index.values.map(RowIndex(_))
}

/** Column index value. */
opaque type ColumnIndex = Index

object ColumnIndex {
  def apply(raw: Index): ColumnIndex = raw

  extension (columnIndex: ColumnIndex)
    def raw: Index = columnIndex: Index

  val values: IndexedSeq[ColumnIndex] = Index.values.map(ColumnIndex(_))
}

// ?? TOOD:  Investigate Refined and/or Scala bug:  Having extension method
//   here (not in object RowIndex) using name "value" causes weird error at
//   unwrapping call, even if differently-named extension method exists
//   (at this level of in object RowIndex)and is used for unwrapping call:
//extension (rowIndex: RowIndex)
//  def value: Index = rowIndex: Index
