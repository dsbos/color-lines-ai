package com.us.dsb.colorlines.game.logic

import com.us.dsb.colorlines.game.GameLogicSupport
import com.us.dsb.colorlines.game.Parameters.BoardOrder
import com.us.dsb.colorlines.game.board.{
  BallColor, Board, BoardReadView, CellAddress, CellState, ColumnIndex, Index, RowIndex}

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

import scala.util.Random

class PathCheckerTest extends AnyFunSpec {

  // ????? TODO:  Review w.r.t. tests' using just BoardReadView API vs. not other
  //   Board and GameLogicSupport stuff, so maybe NameThisGameLogicSupportTest
  //   doesn't depend on GameLogicSupport.

  private class SimpleOccupancyBoard(rowStrings: String*) extends BoardReadView {
    Predef.assert(rowStrings.size == BoardOrder,
                  s"need $BoardOrder arguments, but got ${rowStrings.size}")
    Predef.assert(rowStrings.forall(_.length == BoardOrder),
                  s"string lengths be $BoardOrder, but were ${rowStrings.map(_.size)}")
    private val cellStates: IndexedSeq[IndexedSeq[CellState]] =
      rowStrings.toIndexedSeq.map { rowString =>
        rowString.map { cellOccupanyCharacter =>
          cellOccupanyCharacter match {
            case '-'   => CellState.empty
            case 'B'   => CellState.withBallOfColor(BallColor.Yellow)  // any color
            case other => throw IllegalArgumentException(s"Invalid character '$other; use '-' or 'B'")
          }
        }
      }
    override def getCellStateAt(address: CellAddress): CellState =
      cellStates(address.row.raw.value - Index.Origin)(address.column.raw.value - Index.Origin)
    override def hasABallAt(address: CellAddress): Boolean =
      getCellStateAt(address).asOption.isDefined

    override def isFull: Boolean = ???
    override def getOndeckBalls: Iterable[BallColor] = ???
    override def toCompactString: String = ???
  }

  private class OneBallBoard(row: Int, column: Int) extends BoardReadView {
    private val oneBallAddress = CellAddress.fromRaw(row, column)

    override def getCellStateAt(address: CellAddress): CellState =
      if (address == oneBallAddress)
        CellState.withBallOfColor(BallColor.Blue)
      else
        CellState.empty
    override def hasABallAt(address: CellAddress): Boolean =
      getCellStateAt(address).asOption.isDefined

    override def isFull: Boolean = ???
    override def getOndeckBalls: Iterable[BallColor] = ???
    override def toCompactString: String = ???
  }

  describe("pathExists:") {
    import PathChecker.pathExists

    it("minimal path") {
      pathExists(OneBallBoard(1, 1), CellAddress.fromRaw(1, 1), CellAddress.fromRaw(2, 2))
          `shouldBe` true
    }

    it("minimal blocked ball") {
      val spiralBoard =
        SimpleOccupancyBoard("---------",
                             "---------",
                             "---------",
                             "---------",
                             "---------",
                             "---------",
                             "---------",
                             "-------BB",
                             "-------BB")
      pathExists(spiralBoard, CellAddress.fromRaw(9, 9), CellAddress.fromRaw(9, 8))
          `shouldBe` false
    }

    it("ball anywhere on one-ball board can move to anywhere else") {
      for {
        ballRow <- RowIndex.values
        ballColumn <- ColumnIndex.values
      } yield {
        val fromBallAddress = CellAddress(ballRow, ballColumn)
        // ?????? TODO: Maybe use OneBallBoard to avoid depending on Board (not just BoardReadView):
        val board =
          Board.empty.withBallAt(fromBallAddress,
                                 BallColor.values(Random.nextInt(BallColor.values.length)))
        for {
          toRow <- RowIndex.values
          toColumn <- ColumnIndex.values
          toVacancyAddress = CellAddress(toRow, toColumn)
          // don't assert anything about path to where ball already is
          if toVacancyAddress != fromBallAddress
        } yield {
          val actual = pathExists(board, fromBallAddress, toVacancyAddress)
          withClue(s"from $fromBallAddress to $toVacancyAddress") {
            actual shouldBe true
          }
        }
      }
    }

    it("find path for blocky spiral path") {
      // Note:  Tests all 4 neighbor directions.
      val spiralBoard =
        SimpleOccupancyBoard("B--------",
                             "BBBBBBBB-",
                             "-------B-",
                             "-BBBBB-B-",
                             "-B---B-B-",
                             "-B-BBB-B-",
                             "-B-----B-",
                             "-BBBBBBB-",
                             "---------")
      pathExists(spiralBoard, CellAddress.fromRaw(1, 1), CellAddress.fromRaw(5, 5))
          `shouldBe` true
    }

    it("find no path for blocked blocky spiral path") {
      val spiralBoard =
        SimpleOccupancyBoard("B--------",
                             "BBBBBBBB-",
                             "-------B-",
                             "-BBBBB-B-",
                             "-B---BBB-", // blocked at (5, 7)
                             "-B-BBB-B-",
                             "-B-----B-",
                             "-BBBBBBB-",
                             "---------")
      pathExists(spiralBoard, CellAddress.fromRaw(1, 1), CellAddress.fromRaw(5, 5))
          `shouldBe` false
    }

    it("find no path for blocked no-corners spiral path") {
      val spiralBoard =
        SimpleOccupancyBoard("B--------",
                             "BBBBBBB--", // some blocking-line corners vacant
                             "-------B-",
                             "--BBBB-B-",
                             "-B---BB--",
                             "-B-BBB-B-",
                             "-B-----B-",
                             "--BBBBB--",
                             "---------")
      pathExists(spiralBoard, CellAddress.fromRaw(1, 1), CellAddress.fromRaw(5, 5))
          `shouldBe` false
    }

    it("ball can't move across diagonal line (random probe ball location)") {
      given Random()
      lazy val board0 = Board.empty

      // top left (1, 1) to bottom right (N, N)
      def makeDiagonallyDividedBoard: Board = {
        val diagonalAddresses =
          RowIndex.values.zip(ColumnIndex.values).map { (row, column) => CellAddress(row, column) }
        diagonalAddresses.foldLeft(board0) { (board, address) =>
          board.withBallAt(address, GameLogicSupport.pickRandomBallColor())
        }
      }

      val probeBall = GameLogicSupport.pickRandomBallColor()
      val diagonalBoard = makeDiagonallyDividedBoard
      val fromBallAddress = GameLogicSupport.pickRandomEmptyCell(diagonalBoard).get
      val boardWithProbe = diagonalBoard.withBallAt(fromBallAddress, probeBall)

      // transpose ball coordinates to get cell across boundary
      val toVacancyAddress =
        CellAddress(row    = RowIndex(fromBallAddress.column.raw),
                    column = ColumnIndex(fromBallAddress.row.raw))

      val actual = pathExists(boardWithProbe, fromBallAddress, toVacancyAddress)
      actual shouldBe false
    }
  }

}
