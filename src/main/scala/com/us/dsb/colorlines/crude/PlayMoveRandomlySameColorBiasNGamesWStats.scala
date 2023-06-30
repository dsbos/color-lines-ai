package com.us.dsb.colorlines.crude

import com.us.dsb.colorlines.game.{GameLogicSupport, GameState}
import com.us.dsb.colorlines.game.board.{
  BallColor, BoardReadView, CellAddress, ColumnIndex, RowIndex}

import scala.collection.immutable
import scala.util.Random

// ??????? TODO:  Augment PlayManagerNGamesWStats to support 
//  PlayMoveRandomlySameColorBiasNGamesWStats (maybe by passing player some
//  result of previous move).

/**
 * Initial, crude runner to play by just passing (not moving balls), playing
 * N games, computing average score and other statistics
 */
object PlayMoveRandomlySameColorBiasNGamesWStats extends App {
  private val GameCount = 3000

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
