package com.us.dsb.colorlines.crude

import com.us.dsb.colorlines.game.{GameLogicSupport, GameState}
import com.us.dsb.colorlines.game.Parameters.BoardOrder
import com.us.dsb.colorlines.game.board.{CellAddress, Index}

import scala.util.Random

/**
 * Initial, crude runner to play by just passing (not moving balls), playing
 * N games, computing average score and other statistics
 */
object PlayMoveRandomlyNorthwestBiasNGamesWStats extends App {
  private val GameCount = 3000

  private given rng: Random = Random()

  /** @return ending score */
  def playAGame: Int = {
    val initialPlacementResult = GameLogicSupport.getInitialState()
    var gameState: GameState = initialPlacementResult
    var moveCount = 0
    var validMoveCount = 0
    while (! gameState.board.isFull) {
      // ????? TODO:  Use Index.Origin and BoardOrder, or use Index.MinValue and .MaxValue (or some range-size value)?
      // bias:  9 gets higher probably (81 - 64 = 17 out of 81 than 1 (1 - 0 = 0))
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

  private var gameScoresSum = 0
  private var gameScoresSquaredSum = 0
  private var firstNonzeroGameNumber = 0
  private var nonzeroGameCount = 0
  private var highestScore = 0
  private var minNonzeroScore = Int.MaxValue

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
              + f"; scores: mean = $meanScore%5.3f"
              + f", stdDev = $stdDev%5.3f"
              + s", lowest>0 = $minNonzeroScore"
              + s", highest = $highestScore"
              + s", nonzeroCount = $nonzeroGameCount"
              + s"; firstNonzeroGameNumber = $firstNonzeroGameNumber ")
}
