package com.us.dsb.explore.algs.coloredlines.manual.game.board

import com.us.dsb.explore.algs.coloredlines.manual.game.board.CellAddress
import org.scalatest.PrivateMethodTester
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class LowerGameStateTest extends AnyFunSpec {

  describe("LowerGameState$.empty should return board:") {
    lazy val boardPlus = LowerGameState.empty
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

  describe("LowerGameState.toString should render:") {

    it("- empty board") {
      val expected =   // "<---------/---------/.../--------->"
        (1 to BoardOrder).map { _ =>
          columnIndices.map(_ => "-").mkString("")
        }
            .mkString("< <", "/", " + ()>; 0 pts>")
      LowerGameState.empty.toString shouldBe expected
    }
    it("- board with grid balls") (pending)
    it("- board with on-deck balls") (pending)
  }

  // ("it" and "pending" to note without "!!! IGNORED !!!"
  it("LowerGameState.renderMultiline") {
    pending
  }

  // ("it" and "cancel" to note without "!!! IGNORED !!!"
  it("LowerGameState.renderCompactMultiline") {
    pending
  }

}
