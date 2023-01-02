package com.us.dsb.explore.algs.coloredlines.manual.game

import org.scalatest.funspec.AnyFunSpec
import cats.syntax.option._

class XxGameStateTest extends AnyFunSpec {

  describe("XxGameState$?. tryMoveAt") {
//    import Player._
    import scala.language.implicitConversions
    implicit def intToRow(int: Int): RowIndex    = RowIndex(Index.unsafeFrom(int))
    implicit def intToCol(int: Int): ColumnIndex =ColumnIndex(Index.unsafeFrom(int))


    ignore("marks cells") {

    }
    ignore("accepts marking an unmarked cell") {

    }
    describe("Xxrejects marking an already marked cell:") {
//      it("Xxother player") {
//      }
//      it("Xxsame player") {
//      }
    }
    describe("Xxdetects wins:") {
      it("Xxone case") {
      }
      ignore("do more cases") {
      }
    }
    describe("Xxdetects draws") {
//      it("Xxone case") {
//      }
      ignore("do more cases") {
      }
    }


  }

}
