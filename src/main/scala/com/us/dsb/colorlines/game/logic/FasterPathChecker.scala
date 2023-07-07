package com.us.dsb.colorlines.game.logic

import com.us.dsb.colorlines.game.Parameters
import com.us.dsb.colorlines.game.board.*

import cats.syntax.option.*

import scala.annotation.tailrec

/**
 * Direction-aware depth-first path-existence detector.  (Neighbor in direction
 * of target is traversed first, which works well when board is sparse.)
 *
 * Usually faster than breadth-first SimplePathChecker.  About 3 times faster
 * earlier in game (when board is sparse).  Less gain as board fills; slower in
 * certain cases.)
 *
 * Optimized to avoid most object allocation (e.g., uses mutable data structures).
 *
 */
object FasterPathChecker extends PathChecker {

  // Optimization:  Pulled out to here (static) to reduce allocations:
  // Optimization:  .reverse applied here to reduce allocations:
  private object NeighborhoodOffsetOrders {
    val southThenEastPrereversed = List((+1,  0), ( 0, +1), ( 0, -1), (-1,  0)).reverse
    val southThenWestPrereversed = List((+1,  0), ( 0, -1), ( 0, +1), (-1,  0)).reverse
    val northThenEastPrereversed = List((-1,  0), ( 0, +1), ( 0, -1), (+1,  0)).reverse
    val northThenWestPrereversed = List((-1,  0), ( 0, -1), ( 0, +1), (+1,  0)).reverse
    val eastThenSouthPrereversed = List(( 0, +1), (+1,  0), (-1, 0 ), ( 0, -1)).reverse
    val eastThenNorthPrereversed = List(( 0, +1), (-1,  0), (+1, 0 ), ( 0, -1)).reverse
    val westThenSouthPrereversed = List(( 0, -1), (+1,  0), (-1, 0 ), ( 0, +1)).reverse
    val westThenNorthPrereversed = List(( 0, -1), (-1,  0), (+1, 0 ), ( 0, +1)).reverse
  }

  override def pathExists(board: BoardReadView,
                          fromBallCell: CellAddress,
                          toEmptyCell: CellAddress): Boolean = {
    // Optimization:  Using mutable Array to reduce instance allocation:
    /** Whether each cell is blocked from (further) "reaching"--by ball or
     *  already reached in search. */
    val blockedAt: IndexedSeq[Array[Boolean]] =
      RowIndex.values.map { row =>
          ColumnIndex.values.map { column =>
            board.hasABallAt(CellAddress(row, column))
          }.toArray
      }

    // Optimization:  Using mutable stack to reduce instance allocation:
    // Optimization:  Currently implementing stack outselves  (Note:  Unclear
    //  whether faster than using (already-reasonably-optimal) mutable.Stack.):

    val cellsToCheckAndExpandFrom: Array[CellAddress] = {
      // Construct at maximum needed size to avoid a little reallocation:
      val maxStackSize = RowIndex.values.size * ColumnIndex.values.size
      val stack = new Array[CellAddress](maxStackSize)
      stack.update(0, fromBallCell)
      stack
    }
    var stackOccupancy = 1


    @tailrec
    def loopOnNextQueuedAddress: Boolean = {
      if (stackOccupancy == 0) {
        // no more steps/cells to try--no path exists
        false
      }
      else {
        val reachedAddr = cellsToCheckAndExpandFrom(stackOccupancy - 1)
        stackOccupancy -= 1
        //??println(s"to $toEmptyCell;  $reachedAddr popped")
        if (reachedAddr == toEmptyCell) {
          // got to target cell--a path exists
          true
        }
        else {
          val rowDelta = toEmptyCell.row.raw.value - reachedAddr.row.raw.value
          val colDelta = toEmptyCell.column.raw.value - reachedAddr.column.raw.value
          /*
            - row absolute delta bigger than column absolute delta (or same):
              - row delta positive: first (+1, 0)
                - column delta positive: then (0, +1), (0, -1), (-1, 0)
                - column delta negative: then (0, -1), (0, +1), (-1, 0)
              - row delta negative: first (-1, 0)
                - column delta positive: then (0, +1), (0, -1), (+1, 0)
                - column delta negative: then (0, -1), (0, +1), (+1, 0)
            - column absolutedelta bigger than row absolutedelta:
              - column delta positive: first (0, +1)
                - row delta positive: then (+1, 0), (-1, 0), (0, -1)
                - row delta negative: then (-1, 0), (+1, 0), (0, -1)
              - column delta negative: first (0, -1)
                - row delta positive: then (+1, 0), (-1, 0), (0, +1)
                - row delta negative: then (-1, 0), (+1, 0), (0, +1)
          */
          val reverseOrderedNeighborOffsets = {
            import NeighborhoodOffsetOrders.*
            if (rowDelta >= colDelta)
              if (rowDelta > 0)
                if (colDelta > 0)
                  southThenEastPrereversed
                else
                  southThenWestPrereversed
              else
                if (colDelta > 0)
                  northThenEastPrereversed
                else
                  northThenWestPrereversed
            else
              if (colDelta > 0)
                if (rowDelta > 0)
                  eastThenSouthPrereversed
                else
                  eastThenNorthPrereversed
              else
                if (rowDelta > 0)
                  westThenSouthPrereversed
                else
                  westThenNorthPrereversed
          }
          //??println(s"to $toEmptyCell; @$reachedAddr, reverseOrderedNeighborOffsets = $reverseOrderedNeighborOffsets")
          // no path yet--queue up neighbors neither ball-blocked nor already processed
          //neighborOffsets4.foreach { (rowInc, colInc) =>
          reverseOrderedNeighborOffsets.foreach { (rowInc, colInc) =>
            val rowOffset: Int = reachedAddr.row.raw.value    - Index.Origin + rowInc
            val colOffset: Int = reachedAddr.column.raw.value - Index.Origin + colInc
            if (! (0 <= rowOffset && rowOffset < Parameters.BoardOrder
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
              /*??println(s"to $toEmptyCell; @$reachedAddr; inc: <$rowInc, $colInc>, pushing $neighborAddress")
              println(
                blockedAt.map { row =>
                  row.toSeq.map { if (_) "B" else "." }.mkString(" ")
                }.mkString("\n")
              )*/
              stackOccupancy += 1
              cellsToCheckAndExpandFrom.update(stackOccupancy - 1, neighborAddress)
            }
          }
          loopOnNextQueuedAddress
        }
      }
    }

    val pathExists = loopOnNextQueuedAddress
    //??println(s"$fromBallCell-> $toEmptyCell: pathExists = $pathExists")
    pathExists
  }
}

