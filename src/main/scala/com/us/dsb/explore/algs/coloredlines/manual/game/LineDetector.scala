package com.us.dsb.explore.algs.coloredlines.manual.game

import com.us.dsb.explore.algs.coloredlines.manual.game.Board.CellAddress

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object LineDetector {


  private[this] val axisBaseVectors =
    List(
      ( 0, +1),  // W  --> E
      (+1, +1),  // NW --> SE
      (+1,  0),  // N  --> S
      (+1, -1))  // NW --> SW

  //???? add removing in-line balls
  /**
   * @return
   *   None if not line(s) completed; score increment otherwise
   */
  private[game] def scoreMove(board: Board,
                              ballFrom: CellAddress,
                              ballTo: CellAddress
                             ): Option[Int] = {
    val moveBallColor = board.getBallStateAt(ballTo).get //????
    println(s"scoreMove(...$ballTo...).1 scoring move")

    var maxlineBallCount = 0
    val someAxesResults =
      axisBaseVectors.map { case (rowDelta, colDelta) =>
        println()
        println(s"scoreMove(...$ballTo...).2 scanning axis [$rowDelta, $colDelta]")

        var lineBallCount = 1 // the moved ball

        val directions = List(1, -1)
        directions.foreach { direction =>   //?????? map?
            println(s"scoreMove(...$ballTo...).3 direction = $direction")

          var directionBallCount = 0
          var currentRowIndex = ballTo.row.value.value
          var currentColIndex = ballTo.column.value.value
          while ({
            val candidateRowIndex = currentRowIndex + rowDelta * direction //?????? direction
            val candidateColIndex = currentColIndex + colDelta * direction
            println(s"scoreMove(...$ballTo...).4 indices := ($candidateRowIndex, $candidateColIndex)")
            val inRange =
              1 <= candidateRowIndex && candidateRowIndex <= BoardOrder &&
                  1 <= candidateColIndex && candidateColIndex <= BoardOrder
            val continue: Boolean =
              inRange match {
                case false =>
                  println(s"scoreMove(...$ballTo...).4.1 stop - index out of range / off board")
                  false
                case true =>
                  val candidateAddress = CellAddress(RowIndex(Index.unsafeFrom(candidateRowIndex)),
                                                     ColumnIndex(Index.unsafeFrom(candidateColIndex)))
                  println(s"scoreMove(...$ballTo...).4.2 candidateAddress := "  + candidateAddress)
                  board.getBallStateAt(candidateAddress) match {
                    case None =>
                      println(s"scoreMove(...$ballTo...).4.3 stop - empty")
                      false
                    case Some(candidateBallColor) =>
                      candidateBallColor == moveBallColor match {
                        case false =>
                          println(s"scoreMove(...$ballTo...).4.4 stop - different color")
                          false
                        case true =>
                          println(s"scoreMove(...$ballTo...).4.5 match!")
                          currentRowIndex = candidateRowIndex
                          currentColIndex = candidateColIndex
                          directionBallCount += 1
                          lineBallCount += 1
                          true
                      }
                  }
              }
            continue
          })
          {}

          println(s"scoreMove(...$ballTo...).21 directionBallCount = $directionBallCount")
          println()
        }
        println(s"scoreMove(...$ballTo...).22 lineBallCount = " + lineBallCount)
        println()

        if (lineBallCount > maxlineBallCount) {
          maxlineBallCount = lineBallCount
        }
      }
    println(s"scoreMove(...$ballTo...).22 maxlineBallCount = " + maxlineBallCount)
    println()
      //???val moveScore = ballCount * 4 - 10
    None  //????
  }

}
