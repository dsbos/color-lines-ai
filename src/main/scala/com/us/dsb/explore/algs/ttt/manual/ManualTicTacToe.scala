package com.us.dsb.explore.algs.ttt.manual

import com.us.dsb.explore.algs.ttt.manual.ui.GameUI

object ManualTicTacToe extends App {

  val gameResult = GameUI.runGame(GameUI.ColoredConsoleTextIO)  // ?? specify starting user?
  println("Game result: " + gameResult.text)

}
