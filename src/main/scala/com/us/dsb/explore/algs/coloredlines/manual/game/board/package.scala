package com.us.dsb.explore.algs.coloredlines.manual.game.board


// ???? TODO: Maybe move to package "lines" (with LineDetector) (level above
//  raw board state).
//  - Maybe gather multiple parameters together.
//  - Also, maybe  have regular vs. reduced versions, with redirecting version
//    with single source-level switch between them.
/** Order (length) of lines. */
private type LineOrder = 5
private[game] val LineOrder: LineOrder = valueOf[LineOrder]
