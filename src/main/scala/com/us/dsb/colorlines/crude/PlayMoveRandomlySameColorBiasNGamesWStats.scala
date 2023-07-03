package com.us.dsb.colorlines.crude

import com.us.dsb.colorlines.game.GameLogicSupport
import com.us.dsb.colorlines.game.board.{
  BallColor, BoardReadView, CellAddress, ColumnIndex, RowIndex}

import scala.util.Random

/**
 * Runs PlayManagerNGamesWStats, playing by moving balls randomly with bias
 * toward same-color balls.
 */
object PlayMoveRandomlySameColorBiasNGamesWStats extends App {
  private given rng: Random = Random()

  class NameThisPlayer extends Player {
    override def chooseMove(board: BoardReadView): PlayerMove = {

      val colorToAddressTuples: Iterable[(BallColor, CellAddress)] =
        for {
          row <- RowIndex.values
          col <- ColumnIndex.values
          cellAddress = CellAddress(row, col)
          ballColor <- board.getCellStateAt(cellAddress).asOption
        } yield (ballColor, cellAddress)
      val colorToTuplesMap: Map[BallColor, Iterable[(BallColor, CellAddress)]] =
        colorToAddressTuples.groupBy(_._1)
      val colorToCellCountMap: Map[BallColor, Int] = colorToTuplesMap.map(t => (t._1, t._2.size))
      val aHighCountColorAndCount = colorToCellCountMap.maxBy(_._2)
      val highestCount = aHighCountColorAndCount._2
      val aHighCountColor = aHighCountColorAndCount._1

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
          val twoSomeColorBallCells = colorToTuplesMap(aHighCountColor).map(x => x._2).take(2)

          val tempFrom = twoSomeColorBallCells.head
          val tempTo: CellAddress =
            CellAddress(RowIndex.values(rng.nextInt(RowIndex.values.size)),
                        ColumnIndex.values(rng.nextInt(ColumnIndex.values.size)))
          (tempFrom, tempTo)
          //          ???
        }
      val validMove21 = GameLogicSupport.canMoveBall(board, from, to)
      val move =
        if (validMove21) {
          PlayerMove.MoveBall(from, to)
        }
        else {
          val from: CellAddress =
            CellAddress(RowIndex.values(rng.nextInt(RowIndex.values.size)),
                        ColumnIndex.values(rng.nextInt(ColumnIndex.values.size)))
          val to: CellAddress =
            CellAddress(RowIndex.values(rng.nextInt(RowIndex.values.size)),
                        ColumnIndex.values(rng.nextInt(ColumnIndex.values.size)))
          PlayerMove.MoveBall(from, to)
        }
      move
    }
  }

  val player: Player = NameThisPlayer()
  PlayManagerNGamesWStats.run(player)
}
