package com.us.dsb.colorlines.game.logic

import com.us.dsb.colorlines.game.board.{
  BoardOrder, BoardReadView, CellAddress, Index, ColumnIndex, RowIndex}

import cats.syntax.option.*

import scala.annotation.tailrec
import scala.collection.mutable

object PathChecker {

  // Note:  Pulled out of pathExists's looping method to reduce allocations:
  private val neighborOffsets = List((+1, 0), (-1, 0), (0, +1), (0, -1))

  /**
   * Reports whether ball-movement path exists from given starting cell to
   * given target cell.
   * @param board
   * @param fromBallCell
   *   address of starting cell (expected to contain ball (requirement undefined))
   * @param toEmptyCell
   *   address of target cell (expected to be different from starting cell
   *   (requirement undefined); must be empty, at least if not starting cell)
   * @return
   */
  def pathExists(board: BoardReadView,
                 fromBallCell: CellAddress,
                 toEmptyCell: CellAddress): Boolean = {
   // ???? TODO:  Revisit looping/recursion.  (What is class/method that
   //  logically loops until computation-and-control expression returns
   //  termination value (None?)?  How much instance allocation would that do?)

   // Note:  Using mutable Array and mutable.Queue to reduce instance allocation.

   /** Whether each cell is blocked from (further) "reaching"--by ball or
    *  already reached in search. */
    val blockedAt: IndexedSeq[Array[Boolean]] =
      RowIndex.values.map { row =>
          ColumnIndex.values.map { column =>
            board.hasABallAt(CellAddress(row, column))
          }.toArray
      }
    val cellsToExpandFrom = mutable.Queue[CellAddress](fromBallCell)

    @tailrec
    def loopOnNextQueuedAddress: Boolean = {
      // Note:  Using .isEmpty and .dequeue() to avoid dequeueFirst's allocation
      // of Some instances:
      if (cellsToExpandFrom.isEmpty) {
        // no more steps/cells to try--no path exists
        false
      }
      else {
        val reachedAddr = cellsToExpandFrom.dequeue()
        if (reachedAddr == toEmptyCell) {
          // got to target--a path exists
          true
        }
        else {
          // no path yet; queue up neighbors neither ball-blocked nor already processed
          neighborOffsets.foreach { (rowInc, colInc) =>
            val rowOffset: Int = reachedAddr.row.raw.value    - Index.Origin + rowInc
            val colOffset: Int = reachedAddr.column.raw.value - Index.Origin + colInc
            if (! (   0 <= rowOffset && rowOffset < BoardOrder
                   && 0 <= colOffset && colOffset < BoardOrder)) {
              // off board--ignore this offset
            } else if (blockedAt(rowOffset)(colOffset)) {
              // ball-blocked or already traversed--ignore this offset
            }
            else {
              // vacant--record traversed to and queue traversing from cell:
              blockedAt(rowOffset).update(colOffset, true)
              val neighborAddress = CellAddress(RowIndex.values(rowOffset),
                                                ColumnIndex.values(colOffset))
              cellsToExpandFrom.enqueue(neighborAddress)
            }
          }
          loopOnNextQueuedAddress
        }
      }
    }

    val result = loopOnNextQueuedAddress
    result
  }
}

