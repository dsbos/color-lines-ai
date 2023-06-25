package com.us.dsb.explore.algs.coloredlines.manual.game.board

import com.us.dsb.colorlines.game.board.{
  BoardOrder, CellAddress, IndexOrigin, ColumnIndex, RowIndex}

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

class LowerGameStateTest extends AnyFunSpec {

  describe("LowerGameState$.empty should return board:") {
    lazy val gameState = LowerGameState.empty
    it("- with empty board--empty grid cells") {
      RowIndex.values.foreach { row =>
        ColumnIndex.values.foreach { column =>
          val address = CellAddress(row, column)
          assert(gameState.board.getBallStateAt(address).isEmpty)
        }
      }
    }
    it("- with empty board--empty on-deck list") {
      assert(gameState.board.getOndeckBalls.isEmpty)
    }
  }

  describe("LowerGameState.toString should render:") {

    it("- empty board") {
      val expected =   // "<---------/---------/.../--------->"
        (IndexOrigin to BoardOrder).map { _ =>
          ColumnIndex.values.map(_ => "-").mkString("")
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
