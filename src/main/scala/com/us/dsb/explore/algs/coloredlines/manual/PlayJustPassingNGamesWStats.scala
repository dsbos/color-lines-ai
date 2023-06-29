package com.us.dsb.explore.algs.coloredlines.manual

import com.us.dsb.colorlines.game.GameState
import com.us.dsb.explore.algs.coloredlines.manual.game.GameLogicSupport

import scala.util.Random

/**
 * Initial, crude runner to play by just passing (not moving balls), playing
 * N games, computing average score and other statistics
 */
object PlayJustPassingNGamesWStats extends App {
  private val GameCount = 1000

  private given Random()

  /** @return ending score */
  def playAGame: Int = {
    val initialPlacementResult = GameLogicSupport.getInitialState()
    var gameState: GameState = initialPlacementResult
    while (! gameState.board.isFull) {
      gameState = GameLogicSupport.doPass(gameState)
    }
    gameState.getScore
  }

  var gameScoresSum = 0
  var firstNonzeroGameNumber = 0
  var nonzeroGameCount = 0
  var highestScore = 0
  var minNonzeroScore = Int.MaxValue

  (1 to GameCount).foreach { gameNumber =>
    println()
    println(s"@@@@ Game #$gameNumber:")

    val gameScore = playAGame

    gameScoresSum += gameScore
    highestScore = highestScore max gameScore
    if (gameScore > 0) {
      nonzeroGameCount += 1
      minNonzeroScore = minNonzeroScore min gameScore
      if (firstNonzeroGameNumber == 0) {
        firstNonzeroGameNumber = gameNumber
      }
    }
  }

  val meanScore = 1.0 * gameScoresSum / GameCount
  println(s"@@@@@ End:  $GameCount games" +
              f", meanScore = $meanScore%8.3f" +
              s", minNonzeroScore = $minNonzeroScore" +
              s", highestScore = $highestScore" +
              s", nonzeroGameCount = $nonzeroGameCount" +
              s", firstNonzeroGameNumber = $firstNonzeroGameNumber ")
}
