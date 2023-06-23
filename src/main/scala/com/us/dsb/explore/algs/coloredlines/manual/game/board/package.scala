package com.us.dsb.explore.algs.coloredlines.manual.game.board

import com.us.dsb.colorlines.game.board.BoardOrder
import com.us.dsb.colorlines.game.board.{ColumnIndex, Index, IndexOrigin, RowIndex}

import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.numeric.Interval.Closed
import io.estatico.newtype.macros.newtype

// ???? TODO: Maybe move to package "lines" (with LineDetector) (level above
//  raw board state).
//  - Maybe gather multiple parameters together.
//  - Also, maybe  have regular vs. reduced versions, with redirecting version
//    with single source-level switch between them.
/** Order (length) of lines. */
private type LineOrder = 5
private[game] val LineOrder: LineOrder = valueOf[LineOrder]

// ???? TODO:  Use this (tie to color enumeration) or remove.
private type ColorOrder = 7 // original: blue.dark, blue.light, brown, green, purple, red, yellow
private val ColorOrder: ColorOrder = valueOf[ColorOrder]

//???? add utility methods like withColumnAdjustedBy(delta: Int)? (see GameUIState)

// (unsafeFrom that should be okay since based on BoardOrder:)
private[manual] val rowIndices: IndexedSeq[RowIndex] =
  (IndexOrigin to BoardOrder).map(i => RowIndex(Index.unsafeFrom(i)))
private[manual] val columnIndices: IndexedSeq[ColumnIndex] =
  (IndexOrigin to BoardOrder).map(i => ColumnIndex(Index.unsafeFrom(i)))

//?? factor out frequent row-and-column iteration pattern (set of cells, iterate, passing CellAddress



