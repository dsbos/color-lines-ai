package com.us.dsb.explore.algs.coloredlines.manual.game.board

import enumeratum.{Enum, EnumEntry}

import scala.io.AnsiColor

private[game] sealed class BallKind(private[manual] val initial: String,
                                    private[this] val setFgColorSeq: String,
                                    private[this] val setBgColorSeq: String
                                    ) extends EnumEntry {
  private[manual] def getColoredCharSeq(background: Boolean): String =
    (if (background) this.setBgColorSeq else this.setFgColorSeq) +
        initial +
        AnsiColor.RESET
}

private[game] object BallKind extends Enum[BallKind] {
  import AnsiColor._
  // original: blue.dark, blue.light, brown, green, purple, red, yellow
  private[this] case object Blue    extends BallKind("b",  BLUE,    BLUE_B)
  private[this] case object Cyan    extends BallKind("c",  CYAN,    CYAN_B)
  private[this] case object Black   extends BallKind("k",  BLACK,   BLACK_B)
  private[this] case object Green   extends BallKind("g",  GREEN,   GREEN_B)
  private[this] case object Magenta extends BallKind("m",  MAGENTA, MAGENTA_B)
  private[this] case object Red     extends BallKind("r",  RED,     RED_B)
  private[this] case object Yellow  extends BallKind("y",  YELLOW,  YELLOW_B)

  override val values: IndexedSeq[BallKind] = findValues
}
