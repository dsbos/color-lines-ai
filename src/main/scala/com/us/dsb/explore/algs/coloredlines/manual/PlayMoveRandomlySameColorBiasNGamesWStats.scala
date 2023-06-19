package com.us.dsb.explore.algs.coloredlines.manual

import com.us.dsb.explore.algs.coloredlines.manual.game.GameLogicSupport
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{BallColor, CellAddress, LowerGameState, columnIndices, rowIndices}

import scala.collection.immutable
import scala.util.Random

/**
 * Initial, crude runner to play by just passing (not moving balls), playing
 * N games, computing average score and other statistics
 */
object PlayMoveRandomlySameColorBiasNGamesWStats extends App {
  private val GameCount = 1000

  private implicit val rng = new Random

  /** @return ending score */
  def playAGame: Int = {
    val initialPlacementResult = GameLogicSupport.placeInitialBalls(LowerGameState.empty)
    var gameState: LowerGameState = initialPlacementResult.gameState
    var moveCount = 0
    var validMoveCount = 0
    while (! gameState.board.isFull) {

      val colorToCellTuples: Iterable[(BallColor, CellAddress)] =
        for {
          row <- rowIndices
          col <- columnIndices
          cellAddress = CellAddress(row, col)
          ballColor <- gameState.board.getBallStateAt(cellAddress)
        } yield (ballColor, cellAddress)
      val x2: Map[BallColor, Iterable[(BallColor, CellAddress)]] = colorToCellTuples.groupBy(_._1)
      val colorToCellCountMap: Map[BallColor, Int] = x2.map(x => (x._1, x._2.size))
      val aHighCountColorAndCount = colorToCellCountMap.maxBy(x => x._2)
      val aHighCountColor = aHighCountColorAndCount._1
      val highestCount = aHighCountColorAndCount._2
      val (from, to) =
        if (highestCount < 2) {
          // no balls of same color
          val from: CellAddress = CellAddress.fromRaw(1 + rng.nextInt(rowIndices.size),
                                                      1 + rng.nextInt(columnIndices.size))
          val to: CellAddress = CellAddress.fromRaw(1 + rng.nextInt(rowIndices.size),
                                                    1 + rng.nextInt(columnIndices.size))
          (to, from)
        }
        else {
          /*
          - pick two balls of that color
          - pick vacancy adjacent to one of those balls (if any)
          - move other ball to picked vacancy
          * if no such vacancy, fall back to something (different balls of color, different color, random, pass?)
          * other?
          */
          val twoSomeColorBallCells = x2(aHighCountColor).map(x => x._2).take(2)



          val tempFrom = twoSomeColorBallCells.head
          val tempTo: CellAddress = CellAddress.fromRaw(1 + rng.nextInt(rowIndices.size),
                                                    1 + rng.nextInt(columnIndices.size))
          (tempFrom, tempTo)
          //          ???
        }

      val tryMoveResult1 = GameLogicSupport.doTryMoveBall(gameState, from, to)

      val validMove = tryMoveResult1.moveWasValid
      moveCount += 1

      val tryMoveResult3 =
        if (validMove) {
          validMoveCount += 1
          tryMoveResult1
        }
        else {
          val from: CellAddress = CellAddress.fromRaw(1 + rng.nextInt(rowIndices.size),
                                                      1 + rng.nextInt(columnIndices.size))
          val to: CellAddress = CellAddress.fromRaw(1 + rng.nextInt(rowIndices.size),
                                                    1 + rng.nextInt(columnIndices.size))
          val tryMoveResult2 = GameLogicSupport.doTryMoveBall(gameState, from, to)

          val validMove = tryMoveResult2.moveWasValid
          moveCount += 1
          if (validMove) validMoveCount += 1
          tryMoveResult2

        }
      gameState = tryMoveResult3.gameState
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
