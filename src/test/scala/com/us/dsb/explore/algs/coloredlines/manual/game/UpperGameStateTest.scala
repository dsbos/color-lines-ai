package com.us.dsb.explore.algs.coloredlines.manual.game

import com.us.dsb.explore.algs.coloredlines.manual.game.board.{BoardPlus, CellAddress, ColumnIndex, Index, RowIndex, columnIndices, rowIndices}

import org.scalatest.PrivateMethodTester
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._


class UpperGameStateTest extends AnyFunSpec {

  describe("GameState selection:") {
    //???? randomize?
    lazy val someRow = rowIndices.head
    lazy val someCol = columnIndices.head
    lazy val upperGameState0 = UpperGameState.initial()
    lazy val address = CellAddress(someRow, someCol)

    describe("hasAnyCellSelected should:") {
      it("- return false for fresh, initial game state") {
        upperGameState0.hasAnyCellSelected shouldBe false
      }
      it("- return true for game state with selection") {
        val selectedGameState = upperGameState0.withCellSelected(address)
        selectedGameState.hasAnyCellSelected shouldBe true
      }
    }

    describe("withCellSelected should") {
      lazy val selectedGameState = upperGameState0.withCellSelected(address)

      it("- select _something_") {
        selectedGameState.hasAnyCellSelected shouldBe true
      }
      it("- select _specified_ cell") {
        selectedGameState.isSelectedAt(address) shouldBe true
      }
    }

    describe("withNoSelection should:") {
      lazy val selectedGameState = upperGameState0.withCellSelected(address)
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
//    import Player._
    import scala.language.implicitConversions
    implicit def intToRow(int: Int): RowIndex    = RowIndex(Index.unsafeFrom(int))
    implicit def intToCol(int: Int): ColumnIndex = ColumnIndex(Index.unsafeFrom(int))


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
