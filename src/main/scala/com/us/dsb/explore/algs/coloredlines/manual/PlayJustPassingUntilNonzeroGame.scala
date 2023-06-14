package com.us.dsb.explore.algs.coloredlines.manual

import com.us.dsb.explore.algs.coloredlines.manual.game.GameLogicSupport
import com.us.dsb.explore.algs.coloredlines.manual.game.board.LowerGameState
import com.us.dsb.explore.algs.coloredlines.manual.game.lines.LineDetector.BallArrivalResult

import scala.util.Random

/**
 * Initial, crude runner to play by just passing (not moving balls), playing
 * games until getting a non-zero score.
 */
object PlayJustPassingUntilNonzeroGame extends App {
  private implicit val rng = new Random

  var gameCount: Int = 0
  var lastGameScore = -1
  var nonzeroGame: Boolean = false
  do {
    gameCount += 1
    println()
    println(s"@@@@ Game #$gameCount")

    val initialPlacementResult = GameLogicSupport.placeInitialBalls(LowerGameState.empty)(new Random)
    var gameState: LowerGameState = initialPlacementResult.gameState
    while (! gameState.board.isFull) {
      val wholeResult: BallArrivalResult = GameLogicSupport.doPass(gameState)
      gameState = wholeResult.gameState
    }
    lastGameScore = gameState.getScore
    if (0 != gameState.getScore) {
      nonzeroGame = true

    }
  } while (! nonzeroGame)
  println(s"At game #$gameCount, non-zero score of $lastGameScore")

}
