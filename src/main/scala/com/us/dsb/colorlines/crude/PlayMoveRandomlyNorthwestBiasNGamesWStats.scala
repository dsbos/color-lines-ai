package com.us.dsb.colorlines.crude

import com.us.dsb.colorlines.game.Parameters.BoardOrder
import com.us.dsb.colorlines.game.board.{BoardReadView, CellAddress, Index}

import scala.util.Random

/**
 * Runs PlayManagerNGamesWStats, playing by moving balls randomly with bias
 * toward upper left.
 */
object PlayMoveRandomlyNorthwestBiasNGamesWStats extends App {
  private given rng: Random = Random()

  val player: Player = new Player {
    override def chooseMove(board: BoardReadView): PlayerMove = {
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
      PlayerMove.MoveBall(from, to)
    }
  }

  PlayManagerNGamesWStats.run(player)
}
