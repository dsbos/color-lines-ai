package com.us.dsb.colorlines.game.logic

import com.us.dsb.colorlines.game.GameLogicSupport
import com.us.dsb.colorlines.game.Parameters.BoardOrder
import com.us.dsb.colorlines.game.board.{
  BallColor, Board, BoardReadView, CellAddress, CellState, ColumnIndex, Index, RowIndex}

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

import scala.util.Random

open class PathCheckerTest(checker: PathChecker) extends AnyFunSpec {

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
        rowString.map { cellOccupancyCharacter =>
          cellOccupancyCharacter match {
            case '-'   => CellState.empty
            case 'B'   => CellState.withBallOfColor(BallColor.Yellow)  // any color
            case other => throw IllegalArgumentException(s"Invalid character '$other; use '-' or 'B'")
          }
        }
      }
    override def getCellStateAt(address: CellAddress): CellState =
      cellStates(address.row.raw.value - Index.Origin)(address.column.raw.value - Index.Origin)

    override def getBallCount: Int = ???
    override def getOndeckBalls: Iterable[BallColor] = ???
    override def toCompactString: String = ???
  }

  private class OneBallBoard(oneBallAddress: CellAddress) extends BoardReadView {
    def this(row: Int, column: Int) = {
      this(CellAddress.fromRaw(row, column))
    }
    override def getCellStateAt(address: CellAddress): CellState =
      if (address == oneBallAddress)
        CellState.withBallOfColor(BallColor.Blue)
      else
        CellState.empty
    override def getBallCount: Int = ???
    override def getOndeckBalls: Iterable[BallColor] = ???
    override def toCompactString: String = ???
  }

  private def callPathExists(board: BoardReadView, 
                             srcAndTgtRowAndCol: ((Int, Int), (Int, Int))
                            ): Boolean = {
    val ((srcRow, srcCol), (tgtRow, tgtCol)) = srcAndTgtRowAndCol
    checker.pathExists(board,
                       CellAddress.fromRaw(srcRow, srcCol),
                       CellAddress.fromRaw(tgtRow, tgtCol))
  }

  it("minimal path ((1,1)->(1,2))") {
    callPathExists(OneBallBoard(1, 1), (1, 1) -> (1, 2)) shouldBe true
  }

  it("minimal blocked ball") {
    val board =
      SimpleOccupancyBoard("---------",
                           "---------",
                           "---------",
                           "---------",
                           "---------",
                           "---------",
                           "---------",
                           "-------BB",
                           "-------BB")
    callPathExists(board, (9, 9) -> (9, 8)) shouldBe false
  }

  it("minimal blocked target") {
    val board =
      SimpleOccupancyBoard("-------B-",
                           "--------B",
                           "---------",
                           "---------",
                           "---------",
                           "---------",
                           "---------",
                           "---------",
                           "B--------")
    callPathExists(board, (9, 1) -> (1, 9)) shouldBe false
  }

  it("ball anywhere on one-ball board can move to anywhere else") {
    for {
      ballRow <- RowIndex.values
      ballColumn <- ColumnIndex.values
    } yield {
      val fromBallAddress = CellAddress(ballRow, ballColumn)
      val board: BoardReadView = OneBallBoard(fromBallAddress)
      for {
        toRow <- RowIndex.values
        toColumn <- ColumnIndex.values
        toVacancyAddress = CellAddress(toRow, toColumn)
        // don't assert anything about path to where ball already is
        if toVacancyAddress != fromBallAddress
      } yield {
        val actual =  checker.pathExists(board, fromBallAddress, toVacancyAddress)
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
    callPathExists(spiralBoard, (1, 1) -> (5, 5)) shouldBe true
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
    callPathExists(spiralBoard, (1, 1) -> (5, 5)) shouldBe false
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
    callPathExists(spiralBoard, (1, 1) -> (5, 5)) shouldBe false
  }

  it("ball can't move across diagonal line (random probe ball location)") {
    given Random()
    // top left (1, 1) to bottom right (N, N)
    def makeDiagonallyDividedBoard: Board = {
      val diagonalAddresses =
        RowIndex.values.zip(ColumnIndex.values).map { (row, column) => CellAddress(row, column) }
      diagonalAddresses.foldLeft(Board.empty) { (board, address) =>
        board.withBallAt(address, GameLogicSupport.pickRandomBallColor())
      }
    }

    val diagonalBoard = makeDiagonallyDividedBoard
    val probeBall = GameLogicSupport.pickRandomBallColor()
    val fromBallAddress = GameLogicSupport.pickRandomEmptyCell(diagonalBoard).get
    val boardWithProbe = diagonalBoard.withBallAt(fromBallAddress, probeBall)

    // transpose ball coordinates to get some cell on other side of boundary
    val toVacancyAddress =
      CellAddress(row    = RowIndex(fromBallAddress.column.raw),
                  column = ColumnIndex(fromBallAddress.row.raw))

    val actual = checker.pathExists(boardWithProbe, fromBallAddress, toVacancyAddress)
    actual shouldBe false
  }

  describe("re directional optimization (need temp. printlns and manual review)") {
    it("N to S") {callPathExists(OneBallBoard(3, 5), (3, 5) -> (7, 5)) shouldBe true}
    it("S to N") {callPathExists(OneBallBoard(7, 5), (7, 5) -> (3, 5)) shouldBe true}
    it("W to E") {callPathExists(OneBallBoard(5, 3), (5, 3) -> (5, 7)) shouldBe true}
    it("E to W") {callPathExists(OneBallBoard(5, 7), (5, 7) -> (5, 3)) shouldBe true}

    // ???? TODO:  Regularize and round out:

    it("TBD . SE") {
      callPathExists(OneBallBoard(3, 3), (3, 3) -> (7, 7)) shouldBe true
    }
    it("TBD . SSE") {
      callPathExists(OneBallBoard(3, 3), (3, 3) -> (7, 5)) shouldBe true
    }
    it("TBD . NW") {
      callPathExists(OneBallBoard(7, 7), (7, 7) -> (3, 3)) shouldBe true
    }
    it("TBD . S") {
      callPathExists(OneBallBoard(3, 3), (3, 3) -> (7, 3)) shouldBe true
    }
    it("TBD . E") {
      callPathExists(OneBallBoard(3, 3), (3, 3) -> (3, 7)) shouldBe true
    }

    it("see trying to go toward, but going away when needed") {
      val board =
        SimpleOccupancyBoard("B--------",
                             "----B----",
                             "----B----",
                             "-BBBB----",
                             "---------",
                             "---------",
                             "---------",
                             "---BBBBBB",
                             "---------")
      callPathExists(board, (1, 1) -> (9, 9)) shouldBe true
    }

  }


}
