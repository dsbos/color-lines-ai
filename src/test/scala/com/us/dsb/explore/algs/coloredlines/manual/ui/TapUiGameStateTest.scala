package com.us.dsb.explore.algs.coloredlines.manual.ui

import com.us.dsb.colorlines.game.board.{ColumnIndex, Index, RowIndex}
import com.us.dsb.colorlines.game.board.{columnIndices, rowIndices}
import com.us.dsb.explore.algs.coloredlines.manual.game.board.*
import com.us.dsb.explore.algs.coloredlines.manual.ui.TapUiGameState

import org.scalatest.PrivateMethodTester
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

class TapUiGameStateTest extends AnyFunSpec {

  describe("GameState selection:") {
    //???? randomize?
    lazy val someRow = rowIndices.head
    lazy val someCol = columnIndices.head
    lazy val tapUiGameState0 = TapUiGameState.initial()
    lazy val address = CellAddress(someRow, someCol)

    describe("hasAnyCellSelected should:") {
      it("- return false for fresh, initial game state") {
        tapUiGameState0.hasAnyCellSelected shouldBe false
      }
      it("- return true for game state with selection") {
        val selectedGameState = tapUiGameState0.withCellSelected(address)
        selectedGameState.hasAnyCellSelected shouldBe true
      }
    }

    describe("withCellSelected should") {
      lazy val selectedGameState = tapUiGameState0.withCellSelected(address)

      it("- select _something_") {
        selectedGameState.hasAnyCellSelected shouldBe true
      }
      it("- select _specified_ cell") {
        selectedGameState.isSelectedAt(address) shouldBe true
      }
    }

    describe("withNoSelection should:") {
      lazy val selectedGameState = tapUiGameState0.withCellSelected(address)
      lazy val deselectedGameState = selectedGameState.withNoSelection

      it("- deselect (anything)") {
        deselectedGameState.hasAnyCellSelected shouldBe false
      }
      it("- deselect selected cell") {
        deselectedGameState.isSelectedAt(address) shouldBe false
      }
    }
    //???? test works when no selection anyway
    //???? test isSelectedAt matches row/column with withCellSelected
  }
  
  
  describe("XxGameState$?. tryMoveAt") {
//    import Player.*

    // ???? TODO:  Which form do I want? (named or not; "with" vs. "= <function literal>":
    // (see https://docs.scala-lang.org/scala3/book/ca-implicit-conversions.html,
    // https://docs.scala-lang.org/scala3/reference/contextual/givens.html,
    // https://stackoverflow.com/questions/70213042/scala3-as-and-with-keywords-used-with-given):
    given Conversion[Int, RowIndex] with
      def apply(int: Int): RowIndex = RowIndex(Index.unsafeFrom(int))
    given Conversion[Int, ColumnIndex] = int => ColumnIndex(Index.unsafeFrom(int))
    import scala.language.implicitConversions  // re warnings on _uses_

    ignore("marks cells") {

    }
    ignore("accepts marking an unmarked cell") {

    }
    describe("Xxrejects marking an already marked cell:") {
//      it("Xxother player") {
//      }
//      it("Xxsame player") {
//      }
    }
    describe("Xxdetects wins:") {
      it("Xxone case") {
      }
      ignore("do more cases") {
      }
    }
    describe("Xxdetects draws") {
//      it("Xxone case") {
//      }
      ignore("do more cases") {
      }
    }


  }

}
