package com.us.dsb.explore.algs.coloredlines.manual.game

import enumeratum.{Enum, EnumEntry}

import scala.io.AnsiColor

sealed class BallKind(val initial: String,
                      val setFgColorSeq: String,
                      val setBgColorSeq: String
                     ) extends EnumEntry {
  def getColoredCharSeq(background: Boolean): String =
    (if (background) this.setBgColorSeq else this.setFgColorSeq) +
        initial +
        AnsiColor.RESET
}

object BallKind extends Enum[BallKind] {
  // original: blue.dark, blue.light, brown, green, purple, red, yellow
  case object Blue    extends BallKind("b",  AnsiColor.BLUE,    AnsiColor.BLUE_B)
  case object Cyan    extends BallKind("c",  AnsiColor.CYAN,    AnsiColor.CYAN_B)
  case object Black   extends BallKind("k",  AnsiColor.BLACK,   AnsiColor.BLACK_B)
  case object Green   extends BallKind("g",  AnsiColor.GREEN,   AnsiColor.GREEN_B)
  case object Magenta extends BallKind("m",  AnsiColor.MAGENTA, AnsiColor.MAGENTA_B)
  case object Red     extends BallKind("r",  AnsiColor.RED,     AnsiColor.RED_B)
  case object Yellow  extends BallKind("y",  AnsiColor.YELLOW,  AnsiColor.YELLOW_B)

  override val values: IndexedSeq[BallKind] = findValues
}
