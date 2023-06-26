package com.us.dsb.colorlines.game.board

import scala.io.AnsiColor

// ???? TODO:  Probably split tap-state rendering (for tap UI) from ball-color-only
//  rendering (for generic dense rendering).
object BallColorRenderingExtensions {

  private case class RenderingData(initial: String,
                                   ansiSetFgColorSeq: String,
                                   ansiSetBgColorSeq: String)

  private val renderingData: Map[BallColor, RenderingData] = {
    import BallColor.*
    import AnsiColor.*
    Map(
      Blue    -> RenderingData("B", BLUE,    BLUE_B),
      Cyan    -> RenderingData("C", CYAN,    CYAN_B),
      Black   -> RenderingData("K", BLACK,   BLACK_B),
      Green   -> RenderingData("G", GREEN,   GREEN_B),
      Magenta -> RenderingData("M", MAGENTA, MAGENTA_B),
      Red     -> RenderingData("R", RED,     RED_B),
      Yellow  -> RenderingData("Y", YELLOW,  YELLOW_B),
      )
  }

  extension (color: BallColor) {

    /** Gets basic initial-character representation of ball color. */
    def initial: String = renderingData(color).initial

    /** Gets colored initial-character representation of ball color plus tap-UI
     *  selection state. (Character wrapped with ANSI text color escape
     *  sequences.) */
    def getColoredCharSeq(forBackground: Boolean): String =
      (if (forBackground)
        renderingData(color).ansiSetBgColorSeq
      else
        renderingData(color).ansiSetFgColorSeq
          )
      + initial
      + AnsiColor.RESET
  }
}
