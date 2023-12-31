package com.us.dsb.colorlines.game.logic

import com.us.dsb.colorlines.game.Parameters
import com.us.dsb.colorlines.game.board.{BoardReadView, CellAddress, ColumnIndex, Index, RowIndex}

import cats.syntax.option.*

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Simple breadth-first path-existence detector.  (Not aware of target direction.)
 *
 * Optimized to avoid most object allocation (e.g., uses mutable data structures).
 */
object SimplePathChecker extends PathChecker {

  // Optimization:  Pulled out to here (static) to reduce allocations:
  private val neighborOffsets = List((+1, 0), (-1, 0), (0, +1), (0, -1))

  override def pathExists(board: BoardReadView,
                          fromBallCell: CellAddress,
                          toEmptyCell: CellAddress): Boolean = {

   // Optimization:  Mutable Array and mutable.Queue to reduce instance allocation:

   /** Whether each cell is blocked from (further) "reaching"--by ball or
    *  already reached in search. */
    val blockedAt: IndexedSeq[Array[Boolean]] =
      RowIndex.values.map { row =>
          ColumnIndex.values.map { column =>
            board.hasABallAt(CellAddress(row, column))
          }.toArray
      }
    val cellsToCheckAndExpandFrom = {
      // Note: Constructing at maximum needed size to avoid reallocation:
      val maxQueueSize = RowIndex.values.size * ColumnIndex.values.size
      new mutable.Queue[CellAddress](initialSize = maxQueueSize)
    }
    cellsToCheckAndExpandFrom.enqueue(fromBallCell)

    @tailrec
    def loopOnNextQueuedAddress: Boolean = {
      // Optimization:  Using .isEmpty and .dequeue() to avoid dequeueFirst's
      //   allocation of Some instances:
      if (cellsToCheckAndExpandFrom.isEmpty) {
        // no more steps/cells to try--no path exists
        false
      }
      else {
        val reachedAddr = cellsToCheckAndExpandFrom.dequeue()
        if (reachedAddr == toEmptyCell) {
          // got to target cell--a path exists
          true
        }
        else {
          // no path yet--queue up neighbors neither ball-blocked nor already processed
          neighborOffsets.foreach { (rowInc, colInc) =>
            val rowOffset: Int = reachedAddr.row.raw.value    - Index.Origin + rowInc
            val colOffset: Int = reachedAddr.column.raw.value - Index.Origin + colInc
            if (! (   0 <= rowOffset && rowOffset < Parameters.BoardOrder
                   && 0 <= colOffset && colOffset < Parameters.BoardOrder)) {
              // address off board--ignore this offset
            } else if (blockedAt(rowOffset)(colOffset)) {
              // cell ball-blocked or already traversed to/queued--ignore this offset
            }
            else {
              // cell vacant--record traversed to and queue traversing from cell:
              blockedAt(rowOffset).update(colOffset, true)
              val neighborAddress = CellAddress(RowIndex.values(rowOffset),
                                                ColumnIndex.values(colOffset))
              cellsToCheckAndExpandFrom.enqueue(neighborAddress)
            }
          }
          loopOnNextQueuedAddress
        }
      }
    }

    val pathExists = loopOnNextQueuedAddress
    pathExists
  }
}
