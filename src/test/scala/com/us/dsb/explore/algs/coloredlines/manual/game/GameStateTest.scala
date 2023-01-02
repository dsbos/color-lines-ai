package com.us.dsb.explore.algs.coloredlines.manual.game

import org.scalatest.funspec.AnyFunSpec
import cats.syntax.option._

class XxXxGameStateTest extends AnyFunSpec {

  describe("XxGameState$?. tryMoveAt") {
//    import XxxPlayer._
    import scala.language.implicitConversions
    implicit def intToRow(int: Int): RowIndex    = RowIndex(Index.unsafeFrom(int))
    implicit def intToCol(int: Int): ColumnIndex =ColumnIndex(Index.unsafeFrom(int))


    ignore("marks cells") {

    }
    ignore("accepts marking an unmarked cell") {

    }
    describe("Xxrejects marking an already marked cell:") {
//      it("Xxother player") {
//        val reMarkResult =
//          XxGameState.Xxinitial(XxxPlayer.X)
//              .XxxtryMoveAt(1, 1).toOption.get
//              .XxxtryMoveAt(1, 1)
//        assert(reMarkResult.isLeft, s" (reMarkResult = $reMarkResult)")
//      }
//      it("Xxsame player") {
//
//        val reMarkResult =
//          XxGameState.Xxinitial(XxxPlayer.X)
//              .XxxtryMoveAt(1, 1).toOption.get
//              .XxxtryMoveAt(3, 1).toOption.get
//              .XxxtryMoveAt(1, 1)
//        assert(reMarkResult.isLeft, s" (reMarkResult = $reMarkResult)")
//      }
    }
    describe("Xxdetects wins:") {
      it("Xxone case") {
//
//        val winState =
//          XxGameState.Xxinitial(XxxPlayer.X)
//              .XxxtryMoveAt(1, 1).toOption.get
//              .XxxtryMoveAt(3, 1).toOption.get
//              .XxxtryMoveAt(1, 2).toOption.get
//              .XxxtryMoveAt(3, 2).toOption.get
//              .XxxtryMoveAt(1, 3).toOption.get
//        assertResult(XxGameState.XxGameResult.XxxWin(XxxPlayer.X).some, s" (winState = $winState)") {
//          winState.gameResult
//        }
      }
      ignore("do more cases") {
      }
    }
    describe("Xxdetects draws") {
//      it("Xxone case") {
//        val drawState =
//          XxGameState.Xxinitial(XxxPlayer.X)
//              // .tap(s => println(s.board.renderCompactMultiline + "\n"))
//              .XxxtryMoveAt(1, 1).toOption.get
//              .XxxtryMoveAt(2, 1).toOption.get
//              .XxxtryMoveAt(1, 2).toOption.get
//              .XxxtryMoveAt(2, 2).toOption.get
//              .XxxtryMoveAt(3, 1).toOption.get
//              .XxxtryMoveAt(1, 3).toOption.get
//              .XxxtryMoveAt(2, 3).toOption.get
//              .XxxtryMoveAt(3, 3).toOption.get
//              .XxxtryMoveAt(3, 2).toOption.get  // XXO/OOX/XXO
//        assertResult(XxGameState.XxGameResult.XxxDraw.some, s" (drawState = $drawState)") {
//          drawState.gameResult
//        }
//      }
      ignore("do more cases") {
      }
    }


  }

}
