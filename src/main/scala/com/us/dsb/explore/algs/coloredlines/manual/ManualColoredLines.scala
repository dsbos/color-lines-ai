package com.us.dsb.explore.algs.coloredlines.manual

import com.us.dsb.explore.algs.coloredlines.manual.ui.{GameUI, LiveColoredSegregatedConsoleIO}

private[this] object ManualColoredLines extends App {

  private[this] val gameResult = GameUI.runGame(LiveColoredSegregatedConsoleIO)
  println("Game result: " + gameResult.text)

}
