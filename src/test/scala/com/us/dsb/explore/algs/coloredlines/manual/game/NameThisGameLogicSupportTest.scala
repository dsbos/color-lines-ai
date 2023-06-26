package com.us.dsb.explore.algs.coloredlines.manual.game

import com.us.dsb.colorlines.game.board.{CellAddress, ColumnIndex, RowIndex}
import com.us.dsb.explore.algs.coloredlines.manual.game.board.LowerGameState

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

import scala.util.Random

class NameThisGameLogicSupportTest extends AnyFunSpec {

  // ?????? TODO:  Try to add specific tests of pathExist that don't use much
  //  mutation logic.  (Try to use just BoardReadView API and not other Board
  //   and GameLogicSupport stuff.)  (So NameThisGameLogicSupportTest doesn't
  //   depend on GameLogicSupport.)


  describe("pathExists:") {
    given Random()
    lazy val gameState0 = LowerGameState.empty  //????? just Board?

    it("ball on one-ball board can move anywhere") {
      //?? factor out this frequent iteration pattern (set of cells, iterate, passing CellAddress
      RowIndex.values.foreach { ballRow =>
        ColumnIndex.values.foreach { ballColumn =>
          val fromBallAddress = CellAddress(ballRow, ballColumn)
          val gameState =
            gameState0.withBoardWithBallAt(fromBallAddress,
                                           GameLogicSupport.pickRandomBallColor())
          RowIndex.values.foreach { row =>
            ColumnIndex.values.foreach { column =>
              val toVacancyAddress = CellAddress(row, column)
              val pathExists =
                NameThisGameLogicSupport.pathExists(gameState.board, fromBallAddress, toVacancyAddress)
              withClue( s"from $fromBallAddress to $toVacancyAddress") {
                pathExists shouldBe true
              }
            }
          }
        }
      }
    }

    // top left (1, 1) to bottom right (N, N)
    def makeDiagonallyDividedBoardGameState: LowerGameState = {  //????? just Board?
      val diagonalAddresses =
        RowIndex.values.zip(ColumnIndex.values).map { (row, column) => CellAddress(row, column) }
      diagonalAddresses.foldLeft(gameState0) { (board, address) =>
        board.withBoardWithBallAt(address, GameLogicSupport.pickRandomBallColor())
      }
    }

    it("ball can't move across block (complete diagonal; random probe ball location)") {
      val probeBall = GameLogicSupport.pickRandomBallColor()
      val diagonalGameState = makeDiagonallyDividedBoardGameState  //????? just Board?
      val fromBallAddress = GameLogicSupport.pickRandomEmptyCell(diagonalGameState.board).get
      val boardWithProbe = diagonalGameState.withBoardWithBallAt(fromBallAddress, probeBall).board

      // transpose ball coordinates to get cell across boundary
      val toVacancyAddress =
        CellAddress(row    = RowIndex(fromBallAddress.column.raw),
                    column = ColumnIndex(fromBallAddress.row.raw))

      val pathExists =
        NameThisGameLogicSupport.pathExists(boardWithProbe, fromBallAddress, toVacancyAddress)
      pathExists shouldBe false
    }

    it("???NIY") {
      pending
    }
  }


}
