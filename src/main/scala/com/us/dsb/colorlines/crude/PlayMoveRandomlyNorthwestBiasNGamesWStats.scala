package com.us.dsb.colorlines.crude

import com.us.dsb.colorlines.game.board.{BoardReadView, CellAddress, ColumnIndex, Index, RowIndex}

import scala.util.Random

/**
 * Runs PlayManagerNGamesWStats, playing by moving balls randomly with bias
 * toward upper left.
 */
object PlayMoveRandomlyNorthwestBiasNGamesWStats extends App {
  private given rng: Random = Random()

  val player: Player = new Player {
    override def chooseMove(board: BoardReadView): PlayerMove = {
      // Bias via squares:  9 gets higher probabiliy (81 - 64 = 17, out of 81)
      //   than 1 (1 - 0 = 1, out of 81)):
      val rowCountSquared = RowIndex.values.size * RowIndex.values.size
      val colCountSquared = ColumnIndex.values.size * ColumnIndex.values.size
      val fromRow = math.sqrt(rng.nextInt(rowCountSquared)).toInt + Index.Origin
      val fromCol = math.sqrt(rng.nextInt(colCountSquared)).toInt + Index.Origin
      val toRow =
        math.sqrt((rowCountSquared - Index.Origin) - rng.nextInt(rowCountSquared)).toInt
            + Index.Origin
      val toCol =
        math.sqrt((colCountSquared - Index.Origin) - rng.nextInt(colCountSquared)).toInt
            + Index.Origin

      val from = CellAddress.fromRaw(fromRow, fromCol)
      val to = CellAddress.fromRaw(toRow, toCol)
      PlayerMove.MoveBall(from, to)
    }
  }

  PlayManagerNGamesWStats.run(player)
}
