package com.us.dsb.colorlines.game.board

import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.numeric.Interval.Closed
import io.estatico.newtype.macros.newtype

// ???? TODO:  Review:  Should these in package game.board, game, or split (e.g., board vs. lines)?
// ???? TODO:  Review:  Should these be in package or specific object(s) (e.g., BoardIndexing)?
// ?????? TODO:  Assimilate parameterization of added-balls count, scoring, etc.

/** Order (linear size) of board, as type for refined type. */
type BoardOrder = 9

/** Order (linear size) of board (as regular value). */
val BoardOrder: BoardOrder = valueOf[BoardOrder]


/** Board row or column index integer; 1-based; top row, left column are #1. */
type Index = Int Refined Closed[1, BoardOrder]

// ?? TODO 2->3 . refined:  Review refined_3 way to do following:
// ?? TODO 2-?3 . refined:  Investigate Iron refined-types library:
import scala.language.adhocExtensions  // re extending non-"open" Numeric (_3:1.7.2):
object Index extends RefinedTypeOps.Numeric[Index, Int]

/** Row index value. */
opaque type RowIndex = Index
object RowIndex {
  def apply(raw: Index): RowIndex = raw
  extension (rowIndex: RowIndex)
    def raw: Index = rowIndex: Index
}

/** Column index value. */
opaque type ColumnIndex = Index
object ColumnIndex {
  def apply(raw: Index): ColumnIndex = raw
  extension (columnIndex: ColumnIndex)
    def raw: Index = columnIndex: Index
}

// ?? TOOD:  Investigate Refined and/or Scala bug:  Having extension method
//   here (not in object RowIndex) using name "value" causes weird error at
//   unwrapping call, even if differently-named extension method exists
//   (at this level of in object RowIndex)and is used for unwrapping call:
//extension (rowIndex: RowIndex)
//  def value: Index = rowIndex: Index

