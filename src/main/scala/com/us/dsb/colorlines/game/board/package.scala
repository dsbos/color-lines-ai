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

