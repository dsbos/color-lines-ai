package com.us.dsb.explore.algs.ttt.manual

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
        board.markCell(variedPlayer, row, column).toOption.get
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
        if (row.value.value == 2 && column.value.value == 2) {
          board
        }
        else {
          board.markCell(variedPlayer, row, column).toOption.get
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

  describe("Board.markCell") {

    describe("for unmarked cells:") {

      describe("for some specified cell for specified player:") {
        // ??? move lazy-object example to ScalaTest expl.
        object LazySharedBreakpointable {
          val board0 = Board.initial
          val someRow = rowIndices.head
          val someCol = columnIndices.head
          val markResult = board0.markCell(Player.X, someRow, someCol)
        }
        import LazySharedBreakpointable._

        it("should accept operation") {
          assertResult(true, s" (markResult.isRight; markResult = $markResult)") {
             markResult.isRight
           }

        }
        lazy val markedBoard = markResult.toOption.get
        it("should mark cell for player") {

          assertResult(Some(Player.X)) {
            markedBoard.getMarkAt(someRow, someCol)
          }
        }
        it("(should cause change in string renderings)") {
          assert("xxx"+markedBoard.toString != board0.toString)
          assert("xxx"+markedBoard.renderMultiline != board0.renderMultiline)
        }
      }

      // ?? theoretically/possibly, exercise more/all cells
      // ?? theoretically/possibly, check other cells not changed

    }
    describe("for already-marked cells:") {
      it("should reject operation") {
        val someRow = rowIndices.last
        val someCol = columnIndices.last
        val board0 = Board.initial
        val board1 = board0.markCell(Player.X, someRow, someCol).toOption.get
        val result = board1.markCell(Player.X, someRow, someCol)
        assertResult(true) {
          result.isLeft
        }
      }
    }

  }

  it("Board.getMarkAt covered somewhat with markCell") {
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
              .markCell(player, rowIndices(0), ColumnIndex(Index(1)))
              .toOption.get
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
      val oneXboard = board0.markCell(Player.X, someRow, someCol).toOption.get
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
            .markCell(X, 1, 1).toOption.get
            .markCell(O, 3, 1).toOption.get
            .markCell(X, 1, 2).toOption.get
            .markCell(O, 3, 2).toOption.get
            .markCell(X, 1, 3).toOption.get
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
