package com.us.dsb.explore.algs.coloredlines.manual.game.board

import enumeratum.{Enum, EnumEntry}

import scala.io.AnsiColor

// ?? TODO: Clean?: mixes abstract ball color with UI-specific rendering:
/** Ball color. */
private[game] sealed class BallColor(private[manual] val initial: String,
                                     private[this] val setFgColorSeq: String,
                                     private[this] val setBgColorSeq: String
                                     ) extends EnumEntry {
  private[manual] def getColoredCharSeq(forBackground: Boolean): String =
    (if (forBackground) this.setBgColorSeq else this.setFgColorSeq) +
        initial +
        AnsiColor.RESET
}

private[game] object BallColor extends Enum[BallColor] {
  import AnsiColor._
  // original: blue.dark, blue.light, brown, green, purple, red, yellow
  private[this] case object Blue    extends BallColor("b",  BLUE,    BLUE_B)
  private[this] case object Cyan    extends BallColor("c",  CYAN,    CYAN_B)
  private[this] case object Black   extends BallColor("k",  BLACK,   BLACK_B)
  private[this] case object Green   extends BallColor("g",  GREEN,   GREEN_B)
  private[this] case object Magenta extends BallColor("m",  MAGENTA, MAGENTA_B)
  private[this] case object Red     extends BallColor("r",  RED,     RED_B)
  private[this] case object Yellow  extends BallColor("y",  YELLOW,  YELLOW_B)

  override val values: IndexedSeq[BallColor] = findValues
}
