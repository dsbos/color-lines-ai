package com.us.dsb.explore.algs.coloredlines.manual.game

import com.us.dsb.explore.algs.coloredlines.manual.game.Board.CellAddress
import com.us.dsb.explore.algs.coloredlines.manual.game.GameLogicSupport.MoveResult
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._


import scala.util.Random

class GameLogicSupportTest extends AnyFunSpec {

  ignore("pickRandomBallKind") {}

  describe("pickRandomEmptyCell") {
    it("???NIY") {
      pending
    }
  }
  describe("placeInitialBalls") {
    it("???NIY") {
      pending
    }
  }
  describe("interpretTapLocationToTapAction") {
    it("???NIY") {
      pending
    }
  }
  describe("tapAndStateToTapAction") {
    it("???NIY") {
      pending
    }
  }
  describe("placeNextBalls") {
    it("???NIY") {
      pending
    }
  }

  describe("pathExists:") {
    implicit val rng: Random = new Random()
    lazy val board0 = Board.empty

    it("ball on one-ball board can move anywhere") {
      //?? factor out this frequent iteration pattern (set of cells, iterate, passing CellAddress
      rowIndices.foreach { ballRow =>
        columnIndices.foreach { ballColumn =>
          val fromBallAddress = CellAddress(ballRow, ballColumn)
          val board = board0.withCellHavingBall(fromBallAddress,
                                                GameLogicSupport.pickRandomBallKind())
          rowIndices.foreach { row =>
            columnIndices.foreach { column =>
              val toVacancyAddress = CellAddress(row, column)
              GameLogicSupport.pathExists(board, fromBallAddress, toVacancyAddress) shouldBe true
            }
          }
        }
      }
    }

    it("???NIY") {
      pending
    }
  }

  describe("doTryMoveBall") {
    it("???NIY") {
      pending
    }
  }
  describe("doPass") {
    it("???NIY") {
      pending
    }
  }




}
