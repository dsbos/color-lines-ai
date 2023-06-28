package com.us.dsb.colorlines.game

import com.us.dsb.colorlines.game.board.*

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

import scala.runtime.stdLibPatches.Predef.assert

class GameStateTest extends AnyFunSpec {

  describe("GameState$.empty should return board:") {
    lazy val gameState = GameState.empty
    it("- with empty board--empty grid cells") {
      RowIndex.values.foreach { row =>
        ColumnIndex.values.foreach { column =>
          val address = CellAddress(row, column)
          assert(gameState.board.getCellStateAt(address).asOption.isEmpty)
        }
      }
    }
    it("- with empty board--empty on-deck list") {
      assert(gameState.board.getOndeckBalls.isEmpty)
    }
  }

  describe("GameState.toString should render:") {

    it("- empty board") {
      val expected =   // "<---------/---------/.../--------->"
        (Index.Origin to Parameters.BoardOrder).map { _ =>
          ColumnIndex.values.map(_ => "-").mkString("")
        }
            .mkString("< <", "/", " + ()>; 0 pts>")
      GameState.empty.toCompactString shouldBe expected
      GameState.empty.toString shouldBe expected
    }
    it("- board with grid balls") (pending)
    it("- board with on-deck balls") (pending)
  }

  // ("it" and "pending" to note without "!!! IGNORED !!!"
  it("GameState.renderMultiline") {
    pending
  }

  // ("it" and "cancel" to note without "!!! IGNORED !!!"
  it("GameState.renderCompactMultiline") {
    pending
  }

}
