package com.us.dsb.explore.algs.coloredlines.manual.game

import com.us.dsb.explore.algs.coloredlines.manual.game.Board.CellAddress

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object LineDetector {

  private[this] val lineAxisVectors =
    List(
      ( 0, +1),  // W  --> E
      (+1, +1),  // NW --> SE
      (+1,  0),  // N  --> S
      (+1, -1))  // NW --> SW
  private val directionFactors = List(1, -1) // use type of length 2 (refined List?, Tuple2?, some array?)

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
    println(s"scoreMove(... ballTo = $ballTo...).1")

    val newBallRowIndex = ballTo.row.value.value
    val newBallColIndex = ballTo.column.value.value

    //??? maybe save axis vector (for use in ball deletion)
    case class xxAxisResult(axisLineAddedLength: Int,  // length WITHOUT moved ball
                            directionDetails: List[xxDirectionResult])
    case class xxDirectionResult(excursionLength: Int)

    def computeDirectionResult(axisRowDelta: Int,
                               axisColDelta: Int,
                               directionFactor: Int): xxDirectionResult = {
      println(s"??? computeDirectionResult( axis: ($axisRowDelta / $axisColDelta), dir: $directionFactor).1" )
      var excursionLength = 0
      while ( {
        val candidateExcursionLength = excursionLength + 1
        val candidateRowIndex = newBallRowIndex + axisRowDelta * directionFactor * candidateExcursionLength
        val candidateColIndex = newBallColIndex + axisColDelta * directionFactor * candidateExcursionLength
        println(s"??.n.0: candidate address: ($candidateRowIndex / $candidateColIndex)")

        val inRange =
          1 <= candidateRowIndex && candidateRowIndex <= BoardOrder &&
              1 <= candidateColIndex && candidateColIndex <= BoardOrder
        val haveMatchingBall =
          inRange match {
            case false =>
              println("??.n.1: out of range")
              false
            case true =>
              println("??.n.2: in range ...")
              val candidateAddress = CellAddress(RowIndex(Index.unsafeFrom(candidateRowIndex)),
                                                 ColumnIndex(Index.unsafeFrom(candidateColIndex)))
              board.getBallStateAt(candidateAddress) match {
                case None =>
                  println("??.n.2.1: no ball")
                  false
                case Some(candidateBallColor) =>
                  println("??.n.2.2: ball ...")
                  candidateBallColor == moveBallColor match {
                    case false =>
                      println("??.n.2.2.1: ball doesn't match")
                      false
                    case true =>
                      println("??.n.2.2.2: ball matches")
                      true
                  }
              }
          }
        if (haveMatchingBall) {
          excursionLength = candidateExcursionLength
        }
        haveMatchingBall
      }) {}
      val result = xxDirectionResult(excursionLength)  //????
      println(s"??? computeDirectionResult( axis: ($axisRowDelta / $axisColDelta), dir: $directionFactor).2: result = " + result)
      result
    }

    def computeLineAxisResult(axisRowDelta: Int, axisColDelta: Int): xxAxisResult = {
      val directionsResults: List[xxDirectionResult] =
        directionFactors.map { directionFactor =>
          computeDirectionResult(axisRowDelta,
                                 axisColDelta,
                                 directionFactor)
        }
      val axisLineAddedLength = directionsResults.map(_.excursionLength).sum

      val result = xxAxisResult(axisLineAddedLength, directionsResults)  //????
      println("??? computeLineAxisResult: result = " + result)
      result
    }

    val newAxesResults: List[xxAxisResult] =
      lineAxisVectors.map { case (axisRowDelta, axisColDelta) =>
        computeLineAxisResult(axisRowDelta, axisColDelta)

      }
    println(s"??? newAxesResults = $newAxesResults")
    val completedLineAxesResults = newAxesResults.filter(_.axisLineAddedLength + 1 >= LineOrder)
    val result =
    completedLineAxesResults match {
      case Nil =>
        None // return None for score (signal to place 3 more IF ball moved by user
      case linesAxes =>
        val totalBallsBeingRemoved = 1 + linesAxes.map(_.axisLineAddedLength).sum
        val score = totalBallsBeingRemoved * 4 - 10
        ??? //???? remove balls from board
        Some(score)
    }
    println(s"scoreMove: result (any score ) = $result")
    result
  }

}
