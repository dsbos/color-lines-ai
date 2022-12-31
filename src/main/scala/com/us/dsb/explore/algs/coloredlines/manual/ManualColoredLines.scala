package com.us.dsb.explore.algs.coloredlines.manual

import com.us.dsb.explore.algs.coloredlines.manual.ui.{LiveColoredConsoleTextIO, GameUI}

private[this] object ManualColoredLines extends App {

  private[this] val gameResult = GameUI.runGame(LiveColoredConsoleTextIO)
  println("Game result: " + gameResult.text)

}
