package com.us.dsb.explore.algs.ttt.manual

object ManualTicTacToe extends App {

  val gameResult = GameUI.runGame(GameUI.PlainConsoleTextIO)  // ?? specify starting user?
  println("Game result: " + gameResult.text)

}
