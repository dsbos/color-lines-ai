package com.us.dsb.explore.algs.coloredlines.manual

import com.us.dsb.colorlines.game.{GameLogicSupport, GameState}

import scala.util.Random

/**
 * Initial, crude runner to play by just passing (not moving balls), playing
 * games until getting a non-zero score.
 */
object PlayJustPassingUntilNonzeroGame extends App {
  private given Random()

  private var gameCount: Int = 0
  private var lastGameScore = -1
  private var nonzeroGame: Boolean = false

  while ({
    gameCount += 1
    println()
    println(s"@@@@ Game #$gameCount")

    val initialPlacementResult = GameLogicSupport.getInitialState()
    var gameState: GameState = initialPlacementResult
    while (! gameState.board.isFull) {
      gameState = GameLogicSupport.doPass(gameState)
    }
    lastGameScore = gameState.getScore
    if (0 != gameState.getScore) {
      nonzeroGame = true
    }

    ! nonzeroGame
  }) ()
  println(f"@@@@@ End:  At game #$gameCount%2d, non-zero score of $lastGameScore")
}
