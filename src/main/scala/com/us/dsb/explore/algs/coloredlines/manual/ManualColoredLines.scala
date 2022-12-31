package com.us.dsb.explore.algs.coloredlines.manual

import com.us.dsb.explore.algs.coloredlines.manual.ui.{LiveColoredConsoleTextIO, GameUI}

private object ManualColoredLines extends App {

  private val gameResult = GameUI.runGame(LiveColoredConsoleTextIO)
  println("Game result: " + gameResult.text)

}
