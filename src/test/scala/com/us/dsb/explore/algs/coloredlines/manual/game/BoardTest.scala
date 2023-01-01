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
        board.xxwithCellHavingBall(CellAddress(row, column), BallKind.values(index))
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
          board.xxwithCellHavingBall(CellAddress(row, column), BallKind.values(index))
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
          assert(board.xxgetBallStateAt(address).isEmpty)
        }
      }
    }
    it("- with empty on-deck list") {
      assert(board.xxgetOnDeckBalls.isEmpty)
    }
    it("- with no selection") {
      assert(board.xxhasAnyCellSelected == false)
    }
  }


  describe("XxBoard.withNoSelection") {

      it("Xxshould deselect selected cell") {
        val someRow = rowIndices.head
        val someCol = columnIndices.head
        val board0 = Board.empty
        val address = CellAddress(someRow, someCol)
        val selectedBoard = board0.xxwithCellSelected(address)
        val deselectedBoard = selectedBoard.xxwithNoSelection
        assertResult(false) {
          deselectedBoard.xxisSelectedAt(address)
        }
      }
  }

  it("XxBoard.getMarkAt covered somewhat with ?????markCell") {
    cancel()
  }

  describe("XxBoard.toString") {
    describe("Xxshould render board; some cases:") {

      lazy val board0 = Board.empty
      it("Xxempty board") {
        //board0.toString shouldBe "<---------/---------/---------/---------/---------/---------/---------/---------/--------->"
        board0.toString shouldBe "<----/----/----/---->"
      }
      // (Does initialize object XxxPlayer during ScalaTest registration:)
//      (XxxPlayer.X :: XxxPlayer.O :: Nil).foreach { player =>
//        it(s"board with an $player") {
//          board0
//              .XxwithCellMarkedForPlayer(rowIndices(0), ColumnIndex(Index(1)), player)
//              .toString shouldBe s"<$player--------/---------/---------/---------/---------/---------/---------/---------/--------->"
//        }
//      }
      it("Xxsome full board") {
        regularFilledBoard.toString shouldBe "<crgk/ybcr/gkyb/crgk>"
      }
    }
  }

  // ("it" and "cancel" to note without "!!! IGNORED !!!"
  it("XxBoard.renderMultiline") {
    cancel()
  }

  // ("it" and "cancel" to note without "!!! IGNORED !!!"
  it("XxBoard.renderCompactMultiline") {
    cancel()
  }

  describe("XxBoard.xxxisFull") {
    it("Xxshould not detect empty board as full") {
      Board.empty.xxisFull shouldBe false
    }
//    it ("Xxshould not detect 1-move board as  full") {
//      val board0 = Board.Xxinitial
//      val someRow = rowIndices.head
//      val someCol = columnIndices.head
//      val oneXboard = board0.XxwithCellMarkedForPlayer(someRow, someCol, XxxPlayer.X)
//      oneXboard.hasNoMovesLeft shouldBe false
//    }
    it ("Xxshould not detect 8-moves board as full") {
      variedAllButFilledBoard.xxisFull shouldBe(false)
    }
    // ?? theoretically, check other cardinalities
    // ?? theoretically, check other ~permutations

    it("Xxshould detect full board as full") {
      regularFilledBoard.xxisFull shouldBe true
    }
  }

  describe("XxBoard.vectorIndex (private method we want to test directly):") {
    import PrivateMethodTester._
    val vectorIndex = PrivateMethod[Int](Symbol("vectorIndex"))

    it("Xxshould compute 0 for first row, first column") {
      val index = Board.empty invokePrivate vectorIndex(Index(1), columnIndices.head)
      index shouldEqual 0
    }

    it("Xxshould compute array length - 1 for last row, last column") {
      val index = Board.empty invokePrivate vectorIndex(rowIndices.last, Index(4/*????9*/))
      index shouldEqual BoardOrder * BoardOrder - 1
    }

    describe("Xxshould compute indices in row-major order (chosen but ~isolated)") {
      it("Xx1, 2 => 1") {
        Board.empty invokePrivate vectorIndex(Index(1), Index(2)) shouldEqual 1
      }
      it("Xx2, 1 => 3 (Order/row length/# columns)") {
        Board.empty invokePrivate vectorIndex(Index(2), Index(1)) shouldEqual BoardOrder
      }
    }
  }

}
