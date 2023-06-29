package com.us.dsb.colorlines.crude

import com.us.dsb.colorlines.game.{GameLogicSupport, GameState}

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
  var gameScoresSquaredSum = 0
  var firstNonzeroGameNumber = 0
  var nonzeroGameCount = 0
  var highestScore = 0
  var minNonzeroScore = Int.MaxValue

  (1 to GameCount).foreach { gameNumber =>
    println()
    println(s"@@@@ Game #$gameNumber:")

    val gameScore = playAGame

    gameScoresSum += gameScore
    gameScoresSquaredSum += gameScore * gameScore

    highestScore = highestScore max gameScore
    if (gameScore > 0) {
      nonzeroGameCount += 1
      minNonzeroScore = minNonzeroScore min gameScore
      if (firstNonzeroGameNumber == 0) {
        firstNonzeroGameNumber = gameNumber
      }
    }
  }

  val meanScore = gameScoresSum * 1.0 / GameCount
  // (Crude (https://stackoverflow.com/questions/1174984/how-to-efficiently-calculate-a-running-standard-deviation/1175084#1175084):)
  val stdDev = math.sqrt(gameScoresSquaredSum * 1.0 / GameCount - meanScore * meanScore)

  println(s"@@@@@ End:  $GameCount games"
              +
              f"; scores: mean = $meanScore%5.3f" +
              f", stdDev = $stdDev%5.3f" +
              s", lowest != 0 = $minNonzeroScore" +
              s", highest = $highestScore"
              +
              s"; nonzeroGameCount = $nonzeroGameCount"
              +
              s", firstNonzeroGameNumber = $firstNonzeroGameNumber ")
}
