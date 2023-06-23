package com.us.dsb.colorlines.game.board

// ???? TODO:  Review:  Should these in package game.board, game, or split (e.g., board vs. lines)?
// ???? TODO:  Review:  Should these be in package or specific object(s) (e.g., BoardIndexing)?
// ?????? TODO:  Assimilate parameterization of added-balls count, scoring, etc.

/** Order (linear size) of board, as type for refined type. */
type BoardOrder = 9

/** Order (linear size) of board (as regular value). */
val BoardOrder: BoardOrder = valueOf[BoardOrder]

