package com.us.dsb.explore.algs.ttt.manual

import cats.syntax.option._
import cats.syntax.either._
import enumeratum.EnumEntry

import scala.annotation.tailrec

object ManualTicTacToe extends App {




  // ???? move to GameUI
  val initialState =
    // ?? maybe clean getting indices; maybe get from index ranges, not
    //   constructing here (though here exercises refined type_)
    GameUIState(GameState.initial, RowIndex(Index(1)), ColumnIndex(Index(1)))

  val gameResult: GameUI.GameUIResult = GameUI.getAndDoUiCommands(initialState)
  println("Result: " + gameResult.text)

}
