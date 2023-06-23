package com.us.dsb.explore.algs.coloredlines.manual.game.board

import com.us.dsb.colorlines.game.board.{BallColor, BoardOrder, columnIndices, rowIndices}
import com.us.dsb.colorlines.game.board.{ColumnIndex, Index, IndexOrigin, RowIndex}

import org.scalatest.PrivateMethodTester
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

class BoardTest extends AnyFunSpec {

  private lazy val regularFilledBoard = {
    var colorIndex = 0
    rowIndices.foldLeft(Board.empty) { (board, row) =>
      columnIndices.foldLeft(board){ (board, column) =>
        colorIndex = (colorIndex + 1) % BallColor.values.length
        board.withBallAt(CellAddress(row, column), BallColor.values(colorIndex))
      }
    }
  }
  private lazy val variedAllButFilledBoard = {
    var colorIndex = 0
    rowIndices.foldLeft(Board.empty) { (board, row) =>
      columnIndices.foldLeft(board){ (board, column) =>
        if (row.raw.value == 2 && column.raw.value == 2) { // skip one  //??? clear one from regularFilledBoard?
          board
        }
        else {
          colorIndex = (colorIndex + 1) % BallColor.values.length
          board.withBallAt(CellAddress(row, column), BallColor.values(colorIndex))
        }
      }
    }
  }

  describe("Board$.empty should return board:") {
    lazy val board = Board.empty
    it("- with empty grid cells") {
      rowIndices.foreach { row =>
        columnIndices.foreach { column =>
          val address = CellAddress(row, column)
          assert(board.getBallStateAt(address).isEmpty)
        }
      }
    }
    it("- with empty on-deck list") {
      assert(board.getOndeckBalls.isEmpty)
    }
  }

  describe("Board.vectorIndex (private method we want to test directly):") {
    import PrivateMethodTester.*
    val vectorIndex = PrivateMethod[Int](Symbol("vectorIndex"))

    it("should compute 0 for first row, first column") {
      // ?? TODO 2->3 . refined:  Change Index.unsafeFrom back to Index once macros re-exist:
      val address_1_1  = CellAddress(RowIndex(Index.unsafeFrom(IndexOrigin)), columnIndices.head)
      val index = Board.empty `invokePrivate` vectorIndex(address_1_1)
      index shouldEqual 0
    }

    it("should compute array length - 1 for last row, last column") {
      // ?? TODO 2->3 . refined:  Change Index.unsafeFrom back to Index once macros re-exist:
      val address_n_n  = CellAddress(rowIndices.last, ColumnIndex(Index.unsafeFrom(9)))  //????? use BoardOrder?
      val index = Board.empty `invokePrivate` vectorIndex(address_n_n)
      index shouldEqual BoardOrder * BoardOrder - 1
    }

    describe("should compute indices in row-major order (chosen but ~isolated):") {
      it("- (IO 1) row 1 column 3 => (IO 0) vector index 2") {
        val `row 1 column 3` = CellAddress(rowIndices.head, columnIndices(3 - IndexOrigin))
        Board.empty `invokePrivate` vectorIndex(`row 1 column 3`) shouldEqual 3 - IndexOrigin
      }
      it("- (IO 1) row 3 column 1 => (IO 0) vector index 8") {  //????? adjust label?
        val `row 3 column 1` = CellAddress(rowIndices(3 - IndexOrigin), columnIndices.head)
        Board.empty `invokePrivate` vectorIndex(`row 3 column 1`) shouldEqual
            (3 - IndexOrigin) * BoardOrder + (1 - IndexOrigin)
      }
    }
  }

  describe("Board.toString should render:") {

    it("- empty board") {
      val expected =   // "<---------/---------/.../--------- + ()>"
        Index.values.map { _ =>
          columnIndices.map(_ => "-").mkString("")
        }
            .mkString("<", "/", " + ()>")
      Board.empty.toString shouldBe expected
    }
    it("- board with grid balls") (pending)
    it("- board with on-deck balls") (pending)
  }

  describe("Board.isFull") {
    it("should not detect empty board as full") {
      Board.empty.isFull shouldBe false
    }
    it ("should not detect one-space-left board as full") {
      variedAllButFilledBoard.isFull shouldBe false
    }
    it("should detect full board as full") {
      regularFilledBoard.isFull shouldBe true
    }
  }


}
