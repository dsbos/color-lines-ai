package com.us.dsb.explore.algs.ttt.manual

import cats.syntax.option._

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._


//import org.scalatest.funspec._
//import org.scalatest.matchers._

class BoardTest extends AnyFunSpec {


  private lazy val variedFilledBoard = {
    rowIndices.foldLeft(Board.initial) { (board, row) =>
      columnIndices.foldLeft(board){ (board, column) =>
          val variedPlayer: Player =
            (row.value.value + column.value.value) % 2 match {
              case 0 => Player.O
              case 1 => Player.X
            }
        board.withCellMarkedForPlayerxx(row, column, variedPlayer)
      }
    }
  }
  private lazy val variedAllButFilledBoard = {
    rowIndices.foldLeft(Board.initial) { (board, row) =>
      columnIndices.foldLeft(board){ (board, column) =>
          val variedPlayer: Player =
            (row.value.value + column.value.value) % 2 match {
              case 0 => Player.O
              case 1 => Player.X
            }
        if (row.value.value == 2 && column.value.value == 2) { // skip one  //??? clear one from variedFilledBoard?
          board
        }
        else {
          board.withCellMarkedForPlayerxx(row, column, variedPlayer)
        }
      }
    }
  }

  describe("Board$.initial") {
    //import org.scalatest.matchers.should.Matchers._

    it("should return empty board") {
      val board = Board.initial
      rowIndices.foreach {row =>
        columnIndices.foreach { column =>

          // ?? which is clearer ("new" DSL vs. regular code; in code and in failure messages?)?:

          assert(board.getMarkAt(row, column).isEmpty)

          assertResult(None, s" (at row $row, column $column)") {
            board.getMarkAt(row, column)
          }

          board.getMarkAt(row, column) shouldBe empty
          // no: board.getMarkAt(row, column) should be empty
          board.getMarkAt(row, column) should be (empty)

        }
      }
    }
  }

  describe("Board.withCellMarkedForPlayer") {

    describe("for unmarked cells:") {

      describe("for some specified cell for specified player:") {
        // ??? move lazy-object example to ScalaTest expl.
        object LazySharedBreakpointable {
          val board0 = Board.initial
          val someRow = rowIndices.head
          val someCol = columnIndices.head
          val markedBoard = board0.withCellMarkedForPlayerxx(someRow, someCol, Player.X)
        }
        import LazySharedBreakpointable._

        it("should mark cell for player") {
          assertResult(Some(Player.X), s" (markedBoard = $markedBoard)") {
            markedBoard.getMarkAt(someRow, someCol)
          }
        }
        it("(should cause change in string renderings)") {
          assert(markedBoard.toString != board0.toString)
          assert(markedBoard.renderMultiline != board0.renderMultiline)
        }
      }

      // ?? theoretically/possibly, exercise more/all cells
      // ?? theoretically/possibly, check other cells not changed

    }
    describe("for already-marked cells:") {
      it("should blindy mark again (same-player case)") {
        val someRow = rowIndices.head
        val someCol = columnIndices.head
        val board0 = Board.initial
        val board1 = board0.withCellMarkedForPlayerxx(someRow, someCol, Player.X)
        val result = board1.withCellMarkedForPlayerxx(someRow, someCol, Player.X)

        assertResult(Player.X.some) {
          result.getMarkAt(someRow, someCol)
        }
      }
      it("should blindy mark again (other-player case)") {
        val someRow = rowIndices.last
        val someCol = columnIndices.last
        val board0 = Board.initial
        val board1 = board0.withCellMarkedForPlayerxx(someRow, someCol, Player.X)
        val result = board1.withCellMarkedForPlayerxx(someRow, someCol, Player.O)

        assertResult(Player.O.some) {
          result.getMarkAt(someRow, someCol)
        }
      }
    }

  }

  describe("Board.withCellUnmarked") {

      it("should unmark marked cell") {
        val someRow = rowIndices.head
        val someCol = columnIndices.head
        val board0 = Board.initial
        val markedBoard = board0.withCellMarkedForPlayerxx(someRow, someCol, Player.X)
        val unmarkedBoard = markedBoard.withCellUnmarked(someRow, someCol)
        assertResult(None) {
          unmarkedBoard.getMarkAt(someRow, someCol)
        }
      }
  }

  it("Board.getMarkAt covered somewhat with ?????markCell") {
    cancel()
  }

  describe("Board.toString") {
    describe("should render board; some cases:") {

      lazy val board0 = Board.initial
      it("empty board") {
        board0.toString shouldBe "<---/---/--->"
      }
      // (Does initialize object Player during ScalaTest registration:)
      (Player.X :: Player.O :: Nil).foreach { player =>
        it(s"board with an $player") {
          board0
              .withCellMarkedForPlayerxx(rowIndices(0), ColumnIndex(Index(1)), player)
              .toString shouldBe s"<$player--/---/--->"
        }
      }
      it("some full board") {
        variedFilledBoard.toString shouldBe "<OXO/XOX/OXO>"
      }
    }
  }

  // ("it" and "cancel" to note without "!!! IGNORED !!!"
  it("Board.renderMultiline") {
    cancel()
  }

  describe("Board.hasNoMovesLeft") {
    it("should not detect empty board as full") {
      Board.initial.hasNoMovesLeft shouldBe false
    }
    it ("should not detect 1-move board as  full") {
      val board0 = Board.initial
      val someRow = rowIndices.head
      val someCol = columnIndices.head
      val oneXboard = board0.withCellMarkedForPlayerxx(someRow, someCol, Player.X)
      oneXboard.hasNoMovesLeft shouldBe false
    }
    it ("should not detect 8-moves board as  full") {
      variedAllButFilledBoard.hasNoMovesLeft shouldBe(false)
    }
    // ?? theoretically, check other cardinalities
    // ?? theoretically, check other ~permutations

    it("should detect full board as full") {
      variedFilledBoard.hasNoMovesLeft shouldBe true
    }
  }

  describe("Board.hasThreeInARow") {
    import Player._
    import scala.language.implicitConversions
    implicit def intToRow(int: Int) = RowIndex(Index.unsafeFrom(int))
    implicit def intToCol(int: Int) = ColumnIndex(Index.unsafeFrom(int))

    it("should not detect for empty board") {
      val testBoard = Board.initial
      assertResult(false, s" (from .hasThreeInARow(X) for $testBoard)") {
        testBoard.hasThreeInARow(X)
      }
    }

    val `<XXX/---/OO->` = {
        Board.initial
            .withCellMarkedForPlayerxx(1, 1, X)
            .withCellMarkedForPlayerxx(3, 1, O)
            .withCellMarkedForPlayerxx(1, 2, X)
            .withCellMarkedForPlayerxx(3, 2, O)
            .withCellMarkedForPlayerxx(1, 3, X)
    }

    it("should detect for some three-in-a-row case, w/right player") {
      assertResult(true, s" (from .hasThreeInARow(X) for ${`<XXX/---/OO->`})") {
        `<XXX/---/OO->`.hasThreeInARow(X)
      }
    }

    it("should not detect for wrong player") {
      assertResult(false, s" (from .hasThreeInARow(O) for ${`<XXX/---/OO->`})") {
        `<XXX/---/OO->`.hasThreeInARow(O)
      }
    }

    ignore("possibly check all lines (algorithmically)") {

    }

  }

  describe("Board.vectorIndex:") {
    it("how and how hard/disruptive to test private method?") {
      cancel()
    }
  }


}
