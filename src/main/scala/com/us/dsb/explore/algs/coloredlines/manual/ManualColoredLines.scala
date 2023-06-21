package com.us.dsb.explore.algs.coloredlines.manual

import com.us.dsb.explore.algs.coloredlines.manual.ui.{GameUI, LiveColoredSegregatedConsoleIO}

private object ManualColoredLines extends App {

  private val gameResult = GameUI.runGame(LiveColoredSegregatedConsoleIO)
  println("Game result: " + gameResult.text)

}
