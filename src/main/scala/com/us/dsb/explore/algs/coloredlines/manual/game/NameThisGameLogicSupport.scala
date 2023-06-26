package com.us.dsb.explore.algs.coloredlines.manual.game

import com.us.dsb.colorlines.game.board.{
  BoardOrder, BoardReadView, CellAddress, Index, ColumnIndex, RowIndex}

import cats.syntax.option.*

import scala.annotation.tailrec
import scala.collection.mutable

object NameThisGameLogicSupport {


  //???: likely move core algorithm out; possibly move outer code into LowerGameState/Board:
  /**
   * @param toTapCell - must be empty */
  // (was "private" before test calls:)
  private[game] def pathExists(board: BoardReadView,
                               fromBallCell: CellAddress,
                               toEmptyCell: CellAddress): Boolean = {
    //??println(s"pathExists: fromBallCell = $fromBallCell, toTapCell = $toTapCell")
    //???? CLEAN ALL THIS:
    // - mutability (many eliminate, maybe keep for eventually wanted optimization)
    // - looping/recursion and exiting (what is class/method that ~loops until flag return value?_

    // Blocked from (further) "reaching"--by ball or already reached in search.
    val blockedAt: Array[Array[Boolean]] =
      RowIndex.values.map { row =>
          ColumnIndex.values.map { column =>
            board.hasABallAt(CellAddress(row, column))
          }.toArray
      }.toArray
    val cellsToExpandFrom = mutable.Queue[CellAddress](fromBallCell)

    @tailrec
    def loop: Boolean = {
      cellsToExpandFrom.dequeueFirst(_ => true) match {
        case None =>
          // no more steps/cells to try
          false
        case Some(reachedAddr) =>
          if (reachedAddr == toEmptyCell) {
            // there is a path
            true
          }
          else {
            // no path yet; queue up neighbors neither blocked nor already processed
            val neighborOffsets = List((+1, 0), (-1, 0), (0, +1), (0, -1))
            neighborOffsets.foreach { (rowInc, colInc) =>
              val rowOffset: Int = reachedAddr.row.raw.value    - Index.Origin + rowInc
              val colOffset: Int = reachedAddr.column.raw.value - Index.Origin + colInc
              // ???? TODO:  Use Index.MinValue/.MaxValue, etc.?
              if (! (   0 <= rowOffset && rowOffset < BoardOrder
                     && 0 <= colOffset && colOffset < BoardOrder)) {
                // off board
              } else if (blockedAt(rowOffset)(colOffset)) {
                // blocked or traversed
              }
              else {
                // open; note traversed and try neighboring cells:
                blockedAt(rowOffset).update(colOffset, true)
                val neighborAddress = CellAddress(RowIndex.values(rowOffset),
                                                  ColumnIndex.values(colOffset))
                cellsToExpandFrom.enqueue(neighborAddress)
              }
            }
            loop
          }
      }
    }

    loop
  }
}

