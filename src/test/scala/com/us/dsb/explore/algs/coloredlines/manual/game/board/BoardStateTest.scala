package com.us.dsb.explore.algs.coloredlines.manual.game.board

import org.scalatest.PrivateMethodTester
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class BoardStateTest extends AnyFunSpec {

  private[this] lazy val regularFilledBoardState = {
    var index = 0
    rowIndices.foldLeft(BoardState.empty) { (board, row) =>
      columnIndices.foldLeft(board){ (board, column) =>
        index = (index + 1) % BallKind.values.length
        board.withBallAt(CellAddress(row, column), BallKind.values(index))
      }
    }
  }
  private[this] lazy val variedAllButFilledBoardState = {
    var index = 0
    rowIndices.foldLeft(BoardState.empty) { (board, row) =>
      columnIndices.foldLeft(board){ (board, column) =>
        if (row.value.value == 2 && column.value.value == 2) { // skip one  //??? clear one from regularFilledBoardState?
          board
        }
        else {
          index = (index + 1) % BallKind.values.length
          board.withBallAt(CellAddress(row, column), BallKind.values(index))
        }
      }
    }
  }

  describe("BoardState$.empty should return board:") {
    lazy val board = BoardState.empty
    it("- with empty grid cells") {
      rowIndices.foreach { row =>
        columnIndices.foreach { column =>
          val address = CellAddress(row, column)
          assert(board.getBallStateAt(address).isEmpty)
        }
      }
    }
    it("- with empty on-deck list") {
      assert(board.getOnDeckBalls.isEmpty)
    }
  }

  describe("BoardState.vectorIndex (private method we want to test directly):") {
    import PrivateMethodTester._
    val vectorIndex = PrivateMethod[Int](Symbol("vectorIndex"))

    it("should compute 0 for first row, first column") {
      val address_1_1  = CellAddress(RowIndex(Index(1)), columnIndices.head)
      val index = BoardState.empty invokePrivate vectorIndex(address_1_1)
      index shouldEqual 0
    }

    it("should compute array length - 1 for last row, last column") {
      val address_n_n  = CellAddress(rowIndices.last, ColumnIndex(Index(4 /*????9*/)))  //????? use BoardOrder?
      val index = BoardState.empty invokePrivate vectorIndex(address_n_n)
      index shouldEqual BoardOrder * BoardOrder - 1
    }

    describe("should compute indices in row-major order (chosen but ~isolated):") {
      it("- (IO 1) row 1 column 3 => (IO 0) vector index 2") {
        val `row 1 column 3` = CellAddress(rowIndices.head, columnIndices(3 - 1))
        BoardState.empty invokePrivate vectorIndex(`row 1 column 3`) shouldEqual 3 - 1
      }
      it("- (IO 1) row 3 column 1 => (IO 0) vector index 8") {  //????? adjust label?
        val `row 3 column 1` = CellAddress(rowIndices(3 - 1), columnIndices.head)
        BoardState.empty invokePrivate vectorIndex(`row 3 column 1`) shouldEqual
            (3 - 1) * BoardOrder + (1 - 1)
      }
    }
  }

  describe("BoardState.toString should render:") {

    it("- empty board") {
      val expected =   // "<---------/---------/.../--------- + ()>"
        (1 to BoardOrder).map { _ =>
          columnIndices.map(_ => "-").mkString("")
        }
            .mkString("<", "/", " + ()>")
      BoardState.empty.toString shouldBe expected
    }
    it("- board with grid balls") (pending)
    it("- board with on-deck balls") (pending)
  }

  describe("BoardState.isFull") {
    it("should not detect empty board as full") {
      BoardState.empty.isFull shouldBe false
    }
    it ("should not detect one-space-left board as full") {
      variedAllButFilledBoardState.isFull shouldBe(false)
    }
    it("should detect full board as full") {
      regularFilledBoardState.isFull shouldBe true
    }
  }


}
