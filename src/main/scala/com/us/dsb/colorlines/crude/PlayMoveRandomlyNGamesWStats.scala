package com.us.dsb.colorlines.crude

import com.us.dsb.colorlines.game.board.{BoardReadView, CellAddress, ColumnIndex, RowIndex}

import scala.util.Random

/**
 * Runs PlayManagerNGamesWStats, playing by moving balls randomly.
 */
object PlayMoveRandomlyNGamesWStats extends App {
  private val rng = Random()

  val player: Player = new Player {

    override def chooseMove(board: BoardReadView): PlayerMove = {
      // ???? TODO:  Use BoardOrder, Index.MinValue, size of rowIndices/columnIndices, or what??
      val from: CellAddress =
        CellAddress(RowIndex.values(rng.nextInt(RowIndex.values.size)),
                    ColumnIndex.values(rng.nextInt(ColumnIndex.values.size)))
      val to: CellAddress =
        CellAddress(RowIndex.values(rng.nextInt(RowIndex.values.size)),
                    ColumnIndex.values(rng.nextInt(ColumnIndex.values.size)))
      PlayerMove.MoveBall(from, to)
    }
  }

  PlayManagerNGamesWStats.run(player)
}
