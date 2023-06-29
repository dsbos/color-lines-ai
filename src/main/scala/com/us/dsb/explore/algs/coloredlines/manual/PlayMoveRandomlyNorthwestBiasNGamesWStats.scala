package com.us.dsb.explore.algs.coloredlines.manual

import com.us.dsb.colorlines.game.{GameLogicSupport, GameState}
import com.us.dsb.colorlines.game.Parameters.BoardOrder
import com.us.dsb.colorlines.game.board.{CellAddress, Index}

import scala.util.Random

/**
 * Initial, crude runner to play by just passing (not moving balls), playing
 * N games, computing average score and other statistics
 */
object PlayMoveRandomlyNorthwestBiasNGamesWStats extends App {
  private val GameCount = 1000

  private given rng: Random = Random()

  /** @return ending score */
  def playAGame: Int = {
    val initialPlacementResult = GameLogicSupport.getInitialState()
    var gameState: GameState = initialPlacementResult
    var moveCount = 0
    var validMoveCount = 0
    while (! gameState.board.isFull) {
      // ????? TODO:  Use Index.Origin and BoardOrder, or use Index.MinValue and .MaxValue (or some range-size value)?
      // bias:  9 gets higher probabily (81 - 64 = 17 out of 81 than 1 (1 - 0 = 0))
      val fromRow: Int = math.sqrt(rng.nextInt(BoardOrder * BoardOrder)).toInt + Index.Origin
      val fromCol: Int = math.sqrt(rng.nextInt(BoardOrder * BoardOrder)).toInt + Index.Origin
      val toRow: Int = math.sqrt((BoardOrder * BoardOrder - Index.Origin)
                                     - rng.nextInt(BoardOrder * BoardOrder)).toInt + Index.Origin
      val toCol: Int = math.sqrt((BoardOrder * BoardOrder - Index.Origin)
                                     - rng.nextInt(BoardOrder * BoardOrder)).toInt + Index.Origin

      val from = CellAddress.fromRaw(fromRow, fromCol)
      val to = CellAddress.fromRaw(toRow, toCol)

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
