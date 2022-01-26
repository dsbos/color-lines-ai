package com.us.dsb.explore.algs.ttt.manual

import com.us.dsb.explore.algs.ttt.manual.ui.{LiveColoredConsoleTextIO, GameUI}

object ManualTicTacToe extends App {

  val gameResult = GameUI.runGame(LiveColoredConsoleTextIO)
  println("Game result: " + gameResult.text)

}
