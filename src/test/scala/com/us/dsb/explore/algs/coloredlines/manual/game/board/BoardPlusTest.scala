package com.us.dsb.explore.algs.coloredlines.manual.game.board

import com.us.dsb.explore.algs.coloredlines.manual.game.board.CellAddress
import org.scalatest.PrivateMethodTester
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._


//import org.scalatest.funspec._
//import org.scalatest.matchers._

class BoardPlusTest extends AnyFunSpec {

  describe("BoardPlus$.empty should return board:") {
    lazy val boardPlus = BoardPlus.empty
    it("- with empty board--empty grid cells") {
      rowIndices.foreach { row =>
        columnIndices.foreach { column =>
          val address = CellAddress(row, column)
          assert(boardPlus.getBallStateAt(address).isEmpty)
        }
      }
    }
    it("- with empty board--empty on-deck list") {
      assert(boardPlus.boardState.getOnDeckBalls.isEmpty)
    }
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
