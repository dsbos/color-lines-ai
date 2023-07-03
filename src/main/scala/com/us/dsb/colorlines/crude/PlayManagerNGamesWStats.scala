package com.us.dsb.colorlines.crude

import com.us.dsb.colorlines.game.{GameLogicSupport, GameState}
import com.us.dsb.colorlines.game.board.BoardReadView

import scala.util.Random

/**
 * Basic runner to play using given player, playing N games, computing average
 * score and other statistics
 */
object PlayManagerNGamesWStats {
  private val GameCount = 3000
  private given Random()

  def run(player: Player): Unit = {

    /** @return ending score */
    def playAGame: Int = {
      var moveCount = 0
      var validMoveCount = 0

      var gameState: GameState = GameLogicSupport.getInitialState()
      while (! gameState.board.isFull) {
        val move: PlayerMove = player.chooseMove(gameState.board: BoardReadView)
        val moveWasValid =
          move match {
            case PlayerMove.MoveBall(from, to) =>
              val tryMoveResult = GameLogicSupport.doTryMoveBall(gameState, from, to)
              gameState = tryMoveResult.gameState
              tryMoveResult.moveWasValid
            case PlayerMove.Pass =>
              gameState = GameLogicSupport.doPass(gameState)
              true
            case PlayerMove.Quit =>
              ???
          }
        moveCount += 1
        if (moveWasValid) validMoveCount += 1

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
                + f"; scores: mean = $meanScore%5.3f"
                + f", stdDev = $stdDev%5.3f"
                + s", lowest>0 = $minNonzeroScore"
                + s", highest = $highestScore"
                + s", nonzeroCount = $nonzeroGameCount"
                + s"; firstNonzeroGameNumber = $firstNonzeroGameNumber ")
  }
}
