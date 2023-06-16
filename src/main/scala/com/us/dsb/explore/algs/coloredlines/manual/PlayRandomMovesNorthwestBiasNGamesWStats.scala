package com.us.dsb.explore.algs.coloredlines.manual

import com.us.dsb.explore.algs.coloredlines.manual.game.GameLogicSupport
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{CellAddress, LowerGameState, columnIndices, rowIndices}

import scala.util.Random

/**
 * Initial, crude runner to play by just passing (not moving balls), playing
 * N games, computing average score and other statistics
 */
object PlayRandomMovesNorthwestBiasNGamesWStats extends App {
  private val GameCount = 1000

  private implicit val rng = new Random

  /** @return ending score */
  def playAGame: Int = {
    val initialPlacementResult = GameLogicSupport.placeInitialBalls(LowerGameState.empty)
    var gameState: LowerGameState = initialPlacementResult.gameState
    var moveCount = 0
    var validMoveCount = 0
    while (! gameState.board.isFull) {
      val fromRow: Int = math.sqrt(rng.nextInt(9 * 9)).toInt + 1
      val fromCol: Int = math.sqrt(rng.nextInt(9 * 9)).toInt + 1
      val toRow: Int = math.sqrt((9 * 9 - 1) - rng.nextInt(9 * 9)).toInt + 1
      val toCol: Int = math.sqrt((9 * 9 - 1) - rng.nextInt(9 * 9)).toInt + 1

      val from = CellAddress.fromRaw(fromRow, fromCol)
      val to = CellAddress.fromRaw(toRow, toCol)


      def pickLatestBallCell(curStart: Int = 9 * 9): CellAddress = {
        val row = (curStart - 1) / 9 + 1
        val col = (curStart - 1) % 9 + 1
        val from = CellAddress.fromRaw(row, col)
        //println(s"curStart = $curStart, from = $from")
        if (gameState.board.hasABallAt(from)) {
          from
        }
        else {
          pickLatestBallCell(curStart - 1)
        }
      }
      def pickEarliestVacantCell(curStart: Int = 1): CellAddress = {
        val row = (curStart - 1) / 9 + 1
        val col = (curStart - 1) % 9 + 1
        val to = CellAddress.fromRaw(row, col)
        //println(s"curStart = $curStart, to = $to")
        if (! gameState.board.hasABallAt(to)) {
          to
        }
        else {
          pickEarliestVacantCell(curStart + 1)
        }
      }

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
              f", averageScore = $averageScore%8.3f" +
              s", minPositiveScore = $minPositiveScore" +
              s", highestScore = $highestScore" +
              s", nonzeroGameCount = $nonzeroGameCount" +
              s", firstNonzeroGameNumber = $firstNonzeroGameNumber ")


}
