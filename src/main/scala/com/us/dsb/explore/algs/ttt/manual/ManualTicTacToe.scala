package com.us.dsb.explore.algs.ttt.manual

object ManualTicTacToe extends App {

  val gameResult = GameUI.runGame(GameUI.ConsoleUserTextIO)  // ?? specify starting user?
  println("Game result: " + gameResult.text)

}
