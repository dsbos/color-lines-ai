package com.us.dsb.explore.algs.coloredlines.manual

import com.us.dsb.explore.algs.coloredlines.manual.game.GameLogicSupport
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{CellAddress, LowerGameState, columnIndices, rowIndices}
import com.us.dsb.explore.algs.coloredlines.manual.game.lines.LineDetector.BallArrivalResult

import javax.swing.event.CellEditorListener
import scala.util.Random

/**
 * Initial, crude runner to play by just passing (not moving balls), playing
 * N games, computing average score and other statistics
 */
object PlayRandomMovesNGamesWStats extends App {
  private val GameCount = 1000

  private implicit val rng = new Random

  /** @return ending score */
  def playAGame: Int = {
    val initialPlacementResult = GameLogicSupport.placeInitialBalls(LowerGameState.empty)
    var gameState: LowerGameState = initialPlacementResult.gameState
    var moveCount = 0
    var validMoveCount = 0
    while (! gameState.board.isFull) {

      val from: CellAddress = CellAddress.fromRaw(1 + rng.nextInt(rowIndices.size),
                                                  1 + rng.nextInt(columnIndices.size))
      val to: CellAddress = CellAddress.fromRaw(1 + rng.nextInt(rowIndices.size),
                                                1 + rng.nextInt(columnIndices.size))
      val tryMoveResult = GameLogicSupport.doTryMoveBall(gameState, from, to)

      val validMove = tryMoveResult.moveWasValid
      moveCount += 1
      if (validMove) validMoveCount += 1
      gameState = tryMoveResult.gameState
    }
    println(s"@@ playAGame: moveCount = $moveCount" +
                s", validMoveCount = $validMoveCount" +
                f" (${ 100.0 * validMoveCount / moveCount}%1.2f)%%")
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
