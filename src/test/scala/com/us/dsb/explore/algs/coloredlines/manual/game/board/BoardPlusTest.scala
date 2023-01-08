package com.us.dsb.explore.algs.coloredlines.manual.game.board

import com.us.dsb.explore.algs.coloredlines.manual.game.board.CellAddress
import org.scalatest.PrivateMethodTester
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._


//import org.scalatest.funspec._
//import org.scalatest.matchers._

class BoardPlusTest extends AnyFunSpec {

  describe("BoardPlus$.empty should return board:") {
    lazy val board = BoardPlus.empty
    it("- with empty board--empty grid cells") {
      rowIndices.foreach { row =>
        columnIndices.foreach { column =>
          val address = CellAddress(row, column)
          assert(board.getBallStateAt(address).isEmpty)
        }
      }
    }
    it("- with empty board--empty on-deck list") {
      assert(board.getOnDeckBalls.isEmpty)
    }
    it("- with no selection") {
      assert(board.hasAnyCellSelected == false)
    }
  }

  describe("BoardPlus selection:") {
    //???? randomize?
    val someRow = rowIndices.head
    val someCol = columnIndices.head
    val board0 = BoardPlus.empty
    val address = CellAddress(someRow, someCol)

    describe("hasAnyCellSelected should:") {
      it("- return false for fresh, empty board") {
        board0.hasAnyCellSelected shouldBe false
      }
      it("- return true for board with selection") {
        val selectedBoard = board0.withCellSelected(address)
        board0.hasAnyCellSelected shouldBe false
      }
    }

    describe("withCellSelected should") {
      lazy val selectedBoard = board0.withCellSelected(address)

      it("- select _something_") {
        selectedBoard.hasAnyCellSelected shouldBe true
      }
      it("- select _specified_ cell") {
        selectedBoard.isSelectedAt(address) shouldBe true
      }
    }

    describe("withNoSelection should:") {
      lazy val selectedBoard = board0.withCellSelected(address)
      lazy val deselectedBoard = selectedBoard.withNoSelection

      it("- deselect (anything)") {
        deselectedBoard.hasAnyCellSelected shouldBe false
      }
      it("- deselect selected cell") {
        deselectedBoard.isSelectedAt(address) shouldBe false
      }
    }
    //???? test works when no selection anyway
    //???? test isSelectedAt matches row/column with withCellSelected
  }


  describe("BoardPlus.toString should render:") {

    it("- empty board") {
      val expected =   // "<---------/---------/.../--------->"
        (1 to BoardOrder).map { _ =>
          columnIndices.map(_ => "-").mkString("")
        }
            .mkString("< <", "/", " + ()>; 0 pts>")
      BoardPlus.empty.toString shouldBe expected
    }
    it("- board with grid balls") (pending)
    it("- board with on-deck balls") (pending)
  }

  // ("it" and "pending" to note without "!!! IGNORED !!!"
  it("BoardPlus.renderMultiline") {
    pending
  }

  // ("it" and "cancel" to note without "!!! IGNORED !!!"
  it("BoardPlus.renderCompactMultiline") {
    pending
  }

}
