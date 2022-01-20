package com.us.dsb.explore.algs.ttt.manual

import cats.syntax.option._
import cats.syntax.either._
import enumeratum.EnumEntry


object GameState {

  /**
   * Result of completed game.)
   */
  sealed trait GameResult
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
  // ?? maybe add result of move (win/draw/other) with new state (so caller
  //  doesn't have to check state's gameResult;

  def tryMoveAt(row: RowIndex,
                column: ColumnIndex): Either[String, GameState] = {



    // ?? revisit location/division of move/game logic (currently, Board
    //   checks empty cell; board detects wins/draw; GameState uses win/draw
    //   information;
    //   consider allow undo--either Board.markCell would have to allow changing
    //   already-marked cell, or we'd need a separate method for that ~meta-level
    //   operation

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
        else if (markedBoard.hasNoMovesLeft) {
          GameState.GameResult.Draw.some
        }
        else {
          gameResult
        }
      GameState(markedBoard, newGameResult, nextPlayer)
    }
  }

}


