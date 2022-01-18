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

  def initial: GameState = GameState(Boardxx.initial, None)
}

case class GameState(board: Boardxx, gameResult: Option[GameState.GameResult]) {

  // ???? move to GameState
   // ?? later refine from Either[String, ...] to "fancier" error type
   def tryMoveAt(player: Player,
                 row: RowIndex,
                 column: ColumnIndex): Either[String, GameState] = {

     board.markCell(player, row, column).map { markedBoard =>

       val newGameResult =
         if (markedBoard.hasThreeInARow(player)) { // ?? revisit: passing player to be simpler(?)
           println(s"???????????????: Player $player just won")
           GameState.GameResult.Win(player).some
           // ????expand result to flag whether to continue, or check gameResult somewhere
         }
         else {
           //???? check for draw, setting gameState to Draw.some (and connect to termination)
           gameResult
         }
       GameState(markedBoard, newGameResult)
     }
   }

}


