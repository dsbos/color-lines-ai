package com.us.dsb.explore.algs.ttt.manual

import cats.syntax.option._
import cats.syntax.either._
import enumeratum.EnumEntry


// ?? decide "case" or not

object GameState {
  trait GameResult

  object GameResult {
    case object Draw extends GameResult
    case class Win(player: Player) extends GameResult
  }

  def initial: GameState = GameState(Board.initial, None, Player.X)
}

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
         if (markedBoard.hasThreeInARow(currentPlayer)) { // ?? revisit: passing player to be simpler(?)
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


