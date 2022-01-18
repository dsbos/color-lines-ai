package com.us.dsb.explore.algs.ttt.manual

import cats.syntax.option._
import cats.syntax.either._
import enumeratum.EnumEntry


object GameState {

  /**
   * Result of completed game.)
   */
  trait GameResult
  object GameResult {
    case object Draw extends GameResult
    case class Win(player: Player) extends GameResult
  }

  def initial(startingPlayer: Player): GameState =
    GameState(Board.initial, None, startingPlayer)
  def initial: GameState = initial(Player.X)
}

/**
 *
 * @param gameResult  `None` means no win or draw yet
 */
case class GameState(board: Board,
                     gameResult: Option[GameState.GameResult],
                     currentPlayer: Player
                    ) {

   // ?? later refine from Either[String, ...] to "fancier" error type
   def tryMoveAt(row: RowIndex,
                 column: ColumnIndex): Either[String, GameState] = {

     board.markCell(currentPlayer, row, column).map { markedBoard =>
         import Player._
         val nextPlayer = currentPlayer match {
           case X => O
           case O => X
        }

       val newGameResult =
         if (markedBoard.hasThreeInARow(currentPlayer)) {
           GameState.GameResult.Win(currentPlayer).some
         }
         else if (markedBoard.noMovesLeft) {
           GameState.GameResult.Draw.some
         }
         else {
           gameResult
         }
       GameState(markedBoard, newGameResult, nextPlayer)
     }
   }

}


