package com.us.dsb.colorlines.game.board

import scala.io.AnsiColor

/** Ball color/colors. */
enum BallColor derives CanEqual {
  // original: blue.dark, blue.light, brown, green, purple, red, yellow
  case Blue, Cyan, Black, Green, Magenta, Red, Yellow
}
