package com.us.dsb.explore.algs.ttt.manual

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._


//import org.scalatest.funspec._
//import org.scalatest.matchers._

class BoardTest extends AnyFunSpec {




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

        it("should accept command") {
          assertResult(true, s" (markResult.isRight; markResult = $markResult)") {
             markResult.isRight
           }

        }
        it("should mark cell for player") {
          val markedBoard = markResult.toOption.get

          assertResult(Some(Player.X)) {
            markedBoard.getMarkAt(someRow, someCol)
          }
        }
        it("(should cause change in string renderings)") {
          val markedBoard = markResult.toOption.get
          assert(markedBoard.toString != board0.toString)
          assert(markedBoard.renderMultiline != board0.renderMultiline)
        }


      }

      // ?? theoretically/possibly, exercise more/all cells
      // ?? theoretically/possibly, check other cells not changed

    }
    describe("for already-marked cells:") {
      it("should reject marking some already-marked cell:") {
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

  lazy val variedFilledBoard = {
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
  lazy val variedAllButFilledBoard = {
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

  describe("Board.noMovesLeft") {
    it("should detect empty board as not full") {
      Board.initial.noMovesLeft shouldBe false
    }
    it ("should detect one-move board as not full") {
      val board0 = Board.initial
      val someRow = rowIndices.head
      val someCol = columnIndices.head
      val oneXboard = board0.markCell(Player.X, someRow, someCol).toOption.get
      oneXboard.noMovesLeft shouldBe false
    }
    it ("should detect 8-moves board as not full") {
      variedAllButFilledBoard.noMovesLeft shouldBe(false)
    }

    it("should detect full board as full") {
      variedFilledBoard.noMovesLeft shouldBe true
    }
  }

  describe("Board.getMarkAt") {
    ignore("TBD") {
    }
  }




  describe("Board.hasThreeInARow") {
    ignore("TBD") {
    }
  }

  describe("Board.renderMultiline") {
    ignore("TBD") {
    }
  }



  describe("Board.vectorIndex??") {
    ignore("TBD") {
    }
  }


}
