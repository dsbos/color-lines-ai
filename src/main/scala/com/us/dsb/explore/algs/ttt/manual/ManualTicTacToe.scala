package com.us.dsb.explore.algs.ttt.manual

import com.us.dsb.explore.algs.ttt.manual.ui.{ColoredConsoleTextIO, GameUI}

object ManualTicTacToe extends App {

  val gameResult = GameUI.runGame(ColoredConsoleTextIO)  // ?? specify starting user?
  println("Game result: " + gameResult.text)

}
