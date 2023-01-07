package com.us.dsb.explore.algs.coloredlines.manual.game

import cats.syntax.option._
import com.us.dsb.explore.algs.coloredlines.manual.game.BallKind
import com.us.dsb.explore.algs.coloredlines.manual.game.Board.CellAddress
import org.scalatest.PrivateMethodTester
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._


//import org.scalatest.funspec._
//import org.scalatest.matchers._

class BoardTest extends AnyFunSpec {


  private[this] lazy val regularFilledBoard = {
    var index = 0
    rowIndices.foldLeft(Board.empty) { (board, row) =>
      columnIndices.foldLeft(board){ (board, column) =>
        index = (index + 1) % BallKind.values.length
        board.withBallAt(CellAddress(row, column), BallKind.values(index))
      }
    }
  }
  private[this] lazy val variedAllButFilledBoard = {
    var index = 0
    rowIndices.foldLeft(Board.empty) { (board, row) =>
      columnIndices.foldLeft(board){ (board, column) =>
        if (row.value.value == 2 && column.value.value == 2) { // skip one  //??? clear one from regularFilledXxBoard?
          board
        }
        else {
          index = (index + 1) % BallKind.values.length
          board.withBallAt(CellAddress(row, column), BallKind.values(index))
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
      assert(board.getOnDeckBalls.isEmpty)
    }
    it("- with no selection") {
      assert(board.hasAnyCellSelected == false)
    }
  }

  describe("Board.vectorIndex (private method we want to test directly):") {
    import PrivateMethodTester._
    val vectorIndex = PrivateMethod[Int](Symbol("vectorIndex"))

    it("should compute 0 for first row, first column") {
      val address_1_1  = CellAddress(RowIndex(Index(1)), columnIndices.head)
      val index = Board.empty invokePrivate vectorIndex(address_1_1)
      index shouldEqual 0
    }

    it("should compute array length - 1 for last row, last column") {
      val address_n_n  = CellAddress(rowIndices.last, ColumnIndex(Index(4/*????9*/)))
      val index = Board.empty invokePrivate vectorIndex(address_n_n)
      index shouldEqual BoardOrder * BoardOrder - 1
    }

    describe("should compute indices in row-major order (chosen but ~isolated):") {
      it("- (IO 1) row 1 column 3 => (IO 0) vector index 2") {
        val `row 1 column 3`  = CellAddress(rowIndices.head, columnIndices(3 - 1))
        Board.empty invokePrivate vectorIndex(`row 1 column 3`) shouldEqual 3 - 1
      }
      it("- (IO 1) row 3 column 1 => (IO 0) vector index 8") {
        val `row 3 column 1`  = CellAddress(rowIndices(3 - 1), columnIndices.head)
        Board.empty invokePrivate vectorIndex(`row 3 column 1`) shouldEqual
            (3 - 1) * BoardOrder + (1 - 1)
      }
    }
  }

  describe("Board selection:") {
    //???? randomize?
    val someRow = rowIndices.head
    val someCol = columnIndices.head
    val board0 = Board.empty
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


  describe("Board.toString should render:") {

    it("- empty board") {
      val expected =   // "<---------/---------/.../--------->"
        (1 to BoardOrder).map { _ =>
          columnIndices.map(_ => "-").mkString("")
        }
            .mkString("<", "/", ">")
      Board.empty.toString shouldBe expected
    }
    it("- board with grid balls") (pending)
    it("- board with on-deck balls") (pending)
  }

  // ("it" and "pending" to note without "!!! IGNORED !!!"
  it("XxBoard.renderMultiline") {
    pending
  }

  // ("it" and "cancel" to note without "!!! IGNORED !!!"
  it("XxBoard.renderCompactMultiline") {
    pending
  }

  describe("XxBoard.isFull") {
    it("Xxshould not detect empty board as full") {
      Board.empty.isFull shouldBe false
    }
//    it ("Xxshould not detect 1-move board as full") {
//    }
    it ("Xxshould not detect 8-moves board as full") {
      variedAllButFilledBoard.isFull shouldBe(false)
    }
    // ?? theoretically, check other cardinalities
    // ?? theoretically, check other ~permutations

    it("Xxshould detect full board as full") {
      regularFilledBoard.isFull shouldBe true
    }
  }


}
