package com.us.dsb.colorlines.game.logic

import com.us.dsb.colorlines.game.board.{BallColor, Board, BoardOrder, CellAddress, Index}

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

class LineReaperTest extends AnyFunSpec {

  describe("lineAxes") {
    it("anything worth testing?") {}
  }

  describe("relativeDirectionFactors") {
    it("anything worth testing?") {}
  }

  describe("haveMatchingBallAt:") {
    lazy val `R1/C1-only color` = BallColor.values(0)
    lazy val `R2/C2-only color` = BallColor.values(1)
    lazy val board =
      Board.empty
          .withBallAt(CellAddress.fromRaw(1, 1), `R1/C1-only color`)
          .withBallAt(CellAddress.fromRaw(2, 2), `R2/C2-only color`)
    it("should return true for good match (valid index, non-empty cell, same ball color)") {
      LineReaper.haveMatchingBallAt(`R1/C1-only color`, board, Index.Origin, Index.Origin) shouldBe true
      LineReaper.haveMatchingBallAt(`R1/C1-only color`, board, 1, 1) shouldBe true
      LineReaper.haveMatchingBallAt(`R2/C2-only color`, board, 2, 2) shouldBe true
    }
    it("should return false for different color") {
      LineReaper.haveMatchingBallAt(`R2/C2-only color`, board, 1, 1) shouldBe false
      LineReaper.haveMatchingBallAt(`R1/C1-only color`, board, 2, 2) shouldBe false
    }
    it("should return false for empty cell") {
      LineReaper.haveMatchingBallAt(`R2/C2-only color`, board, 1, 2) shouldBe false
      LineReaper.haveMatchingBallAt(`R2/C2-only color`, board, 2, 1) shouldBe false
      LineReaper.haveMatchingBallAt(`R2/C2-only color`, board, 3, 3) shouldBe false
      LineReaper.haveMatchingBallAt(`R1/C1-only color`, board, 3, 3) shouldBe false
    }
    it("should return false for bad coordinates") {
      LineReaper.haveMatchingBallAt(`R1/C1-only color`, board, -1,               1 ) shouldBe false
      LineReaper.haveMatchingBallAt(`R1/C1-only color`, board, 1,                -2) shouldBe false
      LineReaper.haveMatchingBallAt(`R1/C1-only color`, board, BoardOrder + 3,   1 ) shouldBe false
      LineReaper.haveMatchingBallAt(`R1/C1-only color`, board, 1,                BoardOrder + 1 ) shouldBe false
      LineReaper.haveMatchingBallAt(`R1/C1-only color`, board, Index.Origin - 1, Index.Origin    ) shouldBe false
      LineReaper.haveMatchingBallAt(`R1/C1-only color`, board, Index.Origin,     Index.Origin - 1) shouldBe false
    }
  }

  describe("computeDirectionResult") {
    ignore/*it*/("TBD") {}
  }

  describe("computeLineAxisResult") {
    ignore/*it*/("TBD") {}
  }

  describe("removeCompletedLineBalls") {
    ignore/*it*/("TBD") {}
  }

  describe("handleBallArrival") {
    ignore/*it*/("TBD") {}
  }


}
