package com.us.dsb.explore.algs.ttt.manual

import com.us.dsb.explore.algs.ttt.manual.ui.{ColoredConsoleTextIO, GameUI}

object ManualTicTacToe extends App {

  val gameResult = GameUI.runGame(ColoredConsoleTextIO)
  println("Game result: " + gameResult.text)

}
