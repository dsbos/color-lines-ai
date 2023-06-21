package com.us.dsb.explore.algs.coloredlines.manual.game.board

import enumeratum.{Enum, EnumEntry}

import scala.io.AnsiColor

// ?? TODO:  Separate UI-specific rendering from abstract ball color (at least
//  ideally).  Maybe enable "ball.getColoredCharSeq" via extension method and/or
//  type class.
/** Ball color. */
private[manual] sealed class BallColor(private[manual] val initial: String,
                                       private         val setFgColorSeq: String,
                                       private         val setBgColorSeq: String
                                       ) extends EnumEntry {

  /** Gets full populated-cell--state string.  (For cell state plus tap-selection
   * state; character wrapped in ANSI text color escape sequences.) */
  private[manual] def getColoredCharSeq(forBackground: Boolean): String =
    (if (forBackground) setBgColorSeq else setFgColorSeq) +
        initial +
        AnsiColor.RESET
}

private[game] object BallColor extends Enum[BallColor] {
  import AnsiColor.*
  // original: blue.dark, blue.light, brown, green, purple, red, yellow
  private case object Blue    extends BallColor("b",  BLUE,    BLUE_B)
  private case object Cyan    extends BallColor("c",  CYAN,    CYAN_B)
  private case object Black   extends BallColor("k",  BLACK,   BLACK_B)
  private case object Green   extends BallColor("g",  GREEN,   GREEN_B)
  private case object Magenta extends BallColor("m",  MAGENTA, MAGENTA_B)
  private case object Red     extends BallColor("r",  RED,     RED_B)
  private case object Yellow  extends BallColor("y",  YELLOW,  YELLOW_B)

  override val values: IndexedSeq[BallColor] = findValues
}
