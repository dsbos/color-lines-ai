package com.us.dsb.explore.algs.coloredlines.manual

import com.us.dsb.colorlines.game.GameState
import com.us.dsb.colorlines.game.board.{
  BallColor, BoardReadView, CellAddress, ColumnIndex, RowIndex}
import com.us.dsb.explore.algs.coloredlines.manual.game.GameLogicSupport

import scala.collection.immutable
import scala.util.Random

/**
 * Initial, crude runner to play by just passing (not moving balls), playing
 * N games, computing average score and other statistics
 */
object PlayMoveRandomlySameColorBiasNGamesWStats extends App {
  private val GameCount = 1000

  private given rng: Random = Random()

  /** @return ending score */
  def playAGame: Int = {
    val initialPlacementResult = GameLogicSupport.getInitialState()
    var gameState: GameState = initialPlacementResult
    var moveCount = 0
    var validMoveCount = 0
    while (! gameState.board.isFull) {
      val boardView: BoardReadView = gameState.board

      val colorToCellTuples: Iterable[(BallColor, CellAddress)] =
        for {
          row <- RowIndex.values
          col <- ColumnIndex.values
          cellAddress = CellAddress(row, col)
          ballColor <- boardView.getCellStateAt(cellAddress).asOption
        } yield (ballColor, cellAddress)
      val x2: Map[BallColor, Iterable[(BallColor, CellAddress)]] = colorToCellTuples.groupBy(_._1)
      val colorToCellCountMap: Map[BallColor, Int] = x2.map(x => (x._1, x._2.size))
      val aHighCountColorAndCount = colorToCellCountMap.maxBy(x => x._2)
      val aHighCountColor = aHighCountColorAndCount._1
      val highestCount = aHighCountColorAndCount._2
      val (from, to) =
        if (highestCount < 2) {
          // no balls of same color
          val from: CellAddress =
            CellAddress(RowIndex.values(rng.nextInt(RowIndex.values.size)),
                        ColumnIndex.values(rng.nextInt(ColumnIndex.values.size)))
          val to: CellAddress =
            CellAddress(RowIndex.values(rng.nextInt(RowIndex.values.size)),
                        ColumnIndex.values(rng.nextInt(ColumnIndex.values.size)))
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
          val tempTo: CellAddress =
            CellAddress(RowIndex.values(rng.nextInt(RowIndex.values.size)),
                        ColumnIndex.values(rng.nextInt(ColumnIndex.values.size)))
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
          val from: CellAddress =
            CellAddress(RowIndex.values(rng.nextInt(RowIndex.values.size)),
                        ColumnIndex.values(rng.nextInt(ColumnIndex.values.size)))
          val to: CellAddress =
            CellAddress(RowIndex.values(rng.nextInt(RowIndex.values.size)),
                        ColumnIndex.values(rng.nextInt(ColumnIndex.values.size)))
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
