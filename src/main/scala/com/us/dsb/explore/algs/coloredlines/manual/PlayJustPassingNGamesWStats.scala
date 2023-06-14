package com.us.dsb.explore.algs.coloredlines.manual

import com.us.dsb.explore.algs.coloredlines.manual.game.GameLogicSupport
import com.us.dsb.explore.algs.coloredlines.manual.game.board.LowerGameState
import com.us.dsb.explore.algs.coloredlines.manual.game.lines.LineDetector.BallArrivalResult

import scala.util.Random

/**
 * Initial, crude runner to play by just passing (not moving balls), playing
 * N games, computing average score and other statistics
 */
object PlayJustPassingNGamesWStats extends App {
  private val GameCount = 1000

  private implicit val rng = new Random

  /** @return ending score */
  def playAGame: Int = {
    val initialPlacementResult = GameLogicSupport.placeInitialBalls(LowerGameState.empty)
    var gameState: LowerGameState = initialPlacementResult.gameState
    while (! gameState.board.isFull) {
      val wholeResult: BallArrivalResult = GameLogicSupport.doPass(gameState)
      wholeResult.anyRemovals
      gameState = wholeResult.gameState
    }
    gameState.getScore
  }


  var gameScoresSum = 0
  var firstNonzeroGameNumber = 0
  var nonzeroGameCount = 0
  var highestScore = 0
  var minPositiveScore = Int.MaxValue


  (1 to GameCount).foreach { gameNumber =>
    println()
    println(s"@@@@ Game #$gameNumber:")

    val gameScore = playAGame

    gameScoresSum += gameScore
    highestScore = highestScore max gameScore
    if (gameScore > 0) {
      nonzeroGameCount += 1
      minPositiveScore = minPositiveScore min gameScore
      if (firstNonzeroGameNumber == 0) {
        firstNonzeroGameNumber = gameNumber
      }
    }


  }
  val averageScore = 1.0 * gameScoresSum / GameCount
  println(s"@@@@@ End:  $GameCount games" +
              s", averageScore = $averageScore" +
              s", minPositiveScore = $minPositiveScore" +
              s", highestScore = $highestScore" +
              s", nonzeroGameCount = $nonzeroGameCount" +
              s", firstNonzeroGameNumber = $firstNonzeroGameNumber ")


}
