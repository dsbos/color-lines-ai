package com.us.dsb.colorlines.manual

private object ManualColoredLines extends App {

  private val gameResult = GameUI.runGame(LiveColoredSegregatedConsoleIO)
  println("Game result: " + gameResult.text)

}
