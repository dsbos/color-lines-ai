package com.us.dsb.colorlines.manual

import com.us.dsb.colorlines.manual.{GameUI, LiveColoredSegregatedConsoleIO}

private object ManualColoredLines extends App {

  private val gameResult = GameUI.runGame(LiveColoredSegregatedConsoleIO)
  println("Game result: " + gameResult.text)

}
