package com.us.dsb.explore.algs.coloredlines.manual.game

import cats.syntax.option._
import com.us.dsb.explore.algs.coloredlines.manual.game.BallKind
import org.scalatest.PrivateMethodTester
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._


//import org.scalatest.funspec._
//import org.scalatest.matchers._

private[manual] class XxBoardTest extends AnyFunSpec {


  private[this] lazy val regularFilledBoard = {
    var index = 0
    rowIndices.foldLeft(Board.empty) { (board, row) =>
      columnIndices.foldLeft(board){ (board, column) =>
        index = (index + 1) % BallKind.values.length
        board.withCellHavingBall(row, column, BallKind.values(index))
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
          board.withCellHavingBall(row, column, BallKind.values(index))
        }
      }
    }
  }

  describe("Board$.initial") {
    //import org.scalatest.matchers.should.Matchers._

    it("should return empty board") {
      val board = Board.empty
      rowIndices.foreach {row =>
        columnIndices.foreach { column =>

          // ?? which is clearer ("new" DSL vs. regular code; in code and in failure messages?)?:

          assert(board.getBallStateAt(row, column).isEmpty)
          assert(board.isSelectedAt(row, column) == false)

        }
      }
    }
  }

  describe("XxBoard.XxwithCellMarkedForPlayer") {

    describe("for unmarked cells:") {

      describe("for some specified cell for specified player:") {
        // ??? move lazy-object example to ScalaTest expl.
//        object LazySharedBreakpointable {
//          val board0 = Board.Xxinitial
//          val someRow = rowIndices.head
//          val someCol = columnIndices.head
//          val markedXxBoard = board0.XxwithCellMarkedForPlayer(someRow, someCol, XxxPlayer.X)
//        }
//        import LazySharedBreakpointable._
//
//        it("Xxshould mark cell for player") {
//          assertResult(Some(XxxPlayer.X), s" (markedXxBoard = $markedXxBoard)") {
//            markedXxBoard.XxgetMarkAt(someRow, someCol)
//          }
//        }
//        it("Xx(should cause change in string renderings)") {
//          assert(markedXxBoard.toString != board0.toString)
//          assert(markedXxBoard.renderMultiline != board0.renderMultiline)
//        }
      }

      // ?? theoretically/possibly, exercise more/all cells
      // ?? theoretically/possibly, check other cells not changed

    }
    describe("for already-marked cells:") {
      it("Xxshould blindy mark again (same-player case)") {
        val someRow = rowIndices.head
        val someCol = columnIndices.head
        val board0 = Board.empty
//        val board1 = board0.XxwithCellMarkedForPlayer(someRow, someCol, XxxPlayer.X)
//        val result = board1.XxwithCellMarkedForPlayer(someRow, someCol, XxxPlayer.X)
//
//        assertResult(XxxPlayer.X.some) {
//          result.XxgetMarkAt(someRow, someCol)
//        }
      }
      it("Xxshould blindy mark again (other-player case)") {
        val someRow = rowIndices.last
        val someCol = columnIndices.last
        val board0 = Board.empty
//        val board1 = board0.XxwithCellMarkedForPlayer(someRow, someCol, XxxPlayer.X)
//        val result = board1.XxwithCellMarkedForPlayer(someRow, someCol, XxxPlayer.O)
//
//        assertResult(XxxPlayer.O.some) {
//          result.XxgetMarkAt(someRow, someCol)
//        }
      }
    }

  }

  describe("Board.withNoSelection") {

      it("should deselect selected cell") {
        val someRow = rowIndices.head
        val someCol = columnIndices.head
        val board0 = Board.empty
        val selectedBoard = board0.withCellSelected(someRow, someCol)
        val deselectedBoard = selectedBoard.withNoSelection
        assertResult(false) {
          deselectedBoard.isSelectedAt(someRow, someCol)
        }
      }
  }

  it("XxXxBoard.getMarkAt covered somewhat with ?????markCell") {
    cancel()
  }

  describe("XxBoard.toString") {
    describe("should render board; some cases:") {

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
  it("XxXxBoard.renderMultiline") {
    cancel()
  }

  // ("it" and "cancel" to note without "!!! IGNORED !!!"
  it("XxBoard.renderCompactMultiline") {
    cancel()
  }

  describe("Board.xxxisFull") {
    it("should not detect empty board as full") {
      Board.empty.isFull shouldBe false
    }
//    it ("Xxshould not detect 1-move board as  full") {
//      val board0 = Board.Xxinitial
//      val someRow = rowIndices.head
//      val someCol = columnIndices.head
//      val oneXboard = board0.XxwithCellMarkedForPlayer(someRow, someCol, XxxPlayer.X)
//      oneXboard.hasNoMovesLeft shouldBe false
//    }
    it ("Xxshould not detect 8-moves board as full") {
      variedAllButFilledBoard.isFull shouldBe(false)
    }
    // ?? theoretically, check other cardinalities
    // ?? theoretically, check other ~permutations

    it("should detect full board as full") {
      regularFilledBoard.isFull shouldBe true
    }
  }

  describe("XxBoard.hasThreeInARow") {
//    import XxxPlayer._
//    import scala.language.implicitConversions
//    implicit def intToRow(int: Int) = RowIndex(Index.unsafeFrom(int))
//    implicit def intToCol(int: Int) = ColumnIndex(Index.unsafeFrom(int))
//
//    lazy val `<XX-/---/OO->` = {
//      Board.Xxinitial
//          .XxwithCellMarkedForPlayer(1, 1, X)
//          .XxwithCellMarkedForPlayer(3, 1, O)
//          .XxwithCellMarkedForPlayer(1, 2, X)
//          .XxwithCellMarkedForPlayer(3, 2, O)
//    }
//    lazy val `<XXX/---/OO->` = {
//      `<XX-/---/OO->`
//          .XxwithCellMarkedForPlayer(1, 3, X)
//    }
//    lazy val `<XX-/X--/OOO>` = {
//      `<XX-/---/OO->`
//          .XxwithCellMarkedForPlayer(2, 1, X)
//          .XxwithCellMarkedForPlayer(3, 3, O)
//    }
//
//    it("Xxshould not detect for empty board") {
//      val testXxBoard = Board.Xxinitial
//      assertResult(false, s" (from .hasThreeInARow for $testXxBoard)") {
//        testXxBoard.XxxhasThreeInARow
//      }
//    }
//
//    it("Xxshould detect for some three-in-a-row case for X ") {
//      assertResult(true, s" (from .hasThreeInARow for ${`<XXX/---/OO->`})") {
//        `<XXX/---/OO->`.XxxhasThreeInARow
//      }
//    }
//
//    it("Xxshould for detect some three-in-a-row case for O") {
//      assertResult(true, s" (from .hasThreeInARow for ${`<XX-/X--/OOO>`})") {
//        `<XX-/X--/OOO>`.XxxhasThreeInARow
//      }
//    }
//
//    ignore("possibly check all lines (algorithmically)") {
//    }
//
//    ignore("possibly check more/all non-wins (algorithmically)") {
//    }

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

    describe("should compute indices in row-major order (chosen but ~isolated)") {
      it("Xx1, 2 => 1") {
        Board.empty invokePrivate vectorIndex(Index(1), Index(2)) shouldEqual 1
      }
      it("Xx2, 1 => 3 (Order/row length/# columns)") {
        Board.empty invokePrivate vectorIndex(Index(2), Index(1)) shouldEqual BoardOrder
      }
    }
  }

}
