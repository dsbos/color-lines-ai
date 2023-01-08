package com.us.dsb.explore.algs.coloredlines.manual.game.lines

import com.us.dsb.explore.algs.coloredlines.manual.game.board.CellAddress
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{BallKind, BoardPlus}
import com.us.dsb.explore.algs.coloredlines.manual.game.board.BoardOrder
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.PrivateMethodTester
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._


import scala.reflect.internal.util.TableDef.Column
import scala.tools.nsc.doc.base.comment.Cell

class LineDetectorTest extends AnyFunSpec {

  describe("lineAxes") {
    it("anything worth testing?") {}
  }

  describe("relativeDirectionFactors") {
    it("anything worth testing?") {}
  }

  describe("haveMatchingBallAt:") {
    lazy val `R1/C1-only color` = BallKind.values(0)
    lazy val `R2/C2-only color` = BallKind.values(1)
    lazy val sampleBoard =
      BoardPlus.empty
          .withBallAt(CellAddress.fromRaw(1, 1), `R1/C1-only color`)
          .withBallAt(CellAddress.fromRaw(2, 2), `R2/C2-only color`)
    it("should return true for good match (valid index, non-empty cell, same ball color)") {
      LineDetector.haveMatchingBallAt(`R1/C1-only color`, sampleBoard, 1, 1) shouldBe true
      LineDetector.haveMatchingBallAt(`R2/C2-only color`, sampleBoard, 2, 2) shouldBe true
    }
    it("should return false for different color") {
      LineDetector.haveMatchingBallAt(`R2/C2-only color`, sampleBoard, 1, 1) shouldBe false
      LineDetector.haveMatchingBallAt(`R1/C1-only color`, sampleBoard, 2, 2) shouldBe false
    }
    it("should return false for empty cell") {
      LineDetector.haveMatchingBallAt(`R2/C2-only color`, sampleBoard, 1, 2) shouldBe false
      LineDetector.haveMatchingBallAt(`R2/C2-only color`, sampleBoard, 2, 1) shouldBe false
      LineDetector.haveMatchingBallAt(`R2/C2-only color`, sampleBoard, 3, 3) shouldBe false
      LineDetector.haveMatchingBallAt(`R1/C1-only color`, sampleBoard, 3, 3) shouldBe false
    }
    it("should return false for bad coordinates") {
      LineDetector.haveMatchingBallAt(`R1/C1-only color`, sampleBoard, -1,             1 ) shouldBe false
      LineDetector.haveMatchingBallAt(`R1/C1-only color`, sampleBoard, 1,              -2) shouldBe false
      LineDetector.haveMatchingBallAt(`R1/C1-only color`, sampleBoard, BoardOrder + 3, 1 ) shouldBe false
      LineDetector.haveMatchingBallAt(`R1/C1-only color`, sampleBoard, 1,              BoardOrder + 1) shouldBe false
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
