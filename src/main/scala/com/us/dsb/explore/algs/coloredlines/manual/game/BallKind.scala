package com.us.dsb.explore.algs.coloredlines.manual.game

import enumeratum.{Enum, EnumEntry}

import scala.io.AnsiColor

private[game] sealed class BallKind(private val initial: String,
                                    private val setFgColorSeq: String,
                                    private val setBgColorSeq: String
                                   ) extends EnumEntry {
  private[game] def getColoredCharSeq(background: Boolean): String =
    (if (background) this.setBgColorSeq else this.setFgColorSeq) +
        initial +
        AnsiColor.RESET
}

private[game] object BallKind extends Enum[BallKind] {
  // original: blue.dark, blue.light, brown, green, purple, red, yellow
  private case object Blue    extends BallKind("b",  AnsiColor.BLUE,    AnsiColor.BLUE_B)
  private case object Cyan    extends BallKind("c",  AnsiColor.CYAN,    AnsiColor.CYAN_B)
  private case object Black   extends BallKind("k",  AnsiColor.BLACK,   AnsiColor.BLACK_B)
  private case object Green   extends BallKind("g",  AnsiColor.GREEN,   AnsiColor.GREEN_B)
  private case object Magenta extends BallKind("m",  AnsiColor.MAGENTA, AnsiColor.MAGENTA_B)
  private case object Red     extends BallKind("r",  AnsiColor.RED,     AnsiColor.RED_B)
  private case object Yellow  extends BallKind("y",  AnsiColor.YELLOW,  AnsiColor.YELLOW_B)

  override val values: IndexedSeq[BallKind] = findValues
}
