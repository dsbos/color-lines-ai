package com.us.dsb.explore.algs.coloredlines.manual.game

import com.us.dsb.explore.algs.coloredlines.manual.game.Board.CellAddress

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object LineDetector {

  private[this] case class LineAxis(labelArray: String,
                                    rowDelta: Int, // -1 / 0 / 1 (Make refined type?)
                                    colDelta: Int)
  private[this] val lineAxes =
    List(
      LineAxis("→", 0, +1),  // W  --> E    →
      LineAxis("↘",+1, +1),  // NW --> SE   ↘
      LineAxis("↓", 0, +1),  // N  --> S    ↓
      LineAxis("↙",+1, -1))  // NW --> SW   ↙
  private val relativeDirectionFactors = List(1, -1) // use type of length 2 (refined List?, Tuple2?, some array?)

  /**
   * @return
   *   None if no line(s) completed; score increment otherwise
   */
  private[game] def scoreMove(board: Board,
                              ballFrom: CellAddress,
                              ballTo: CellAddress
                             ): Option[Int] = {
    println(s"+scoreMove(... ballTo = $ballTo...).1")
    val moveBallColor = board.getBallStateAt(ballTo).get //????

    val newBallRowIndex = ballTo.row.value.value
    val newBallColIndex = ballTo.column.value.value

    case class RelativeDirectionResult(excursionLength: Int)
    def computeDirectionResult(lineDirectionAxis: LineAxis,
                               directionFactor: Int): RelativeDirectionResult = {
      import lineDirectionAxis._
      println(s"+    computeDirectionResult( axis: $lineDirectionAxis, dir: $directionFactor ).1" )
      var excursionLength = 0
      while ( {
        val candidateExcursionLength = excursionLength + 1
        val candidateRowIndex = newBallRowIndex + rowDelta * directionFactor * candidateExcursionLength
        val candidateColIndex = newBallColIndex + colDelta * directionFactor * candidateExcursionLength
        println(s"    ??.n.0: candidate address: ($candidateRowIndex / $candidateColIndex)")

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
      val result = RelativeDirectionResult(excursionLength)  //????
      println(s"-    computeDirectionResult( axis: $lineDirectionAxis, dir: $directionFactor ).9 result = $result" )
      result
    }

    //??? maybe save axis vector (for use in ball deletion)
    case class AxisResult(axisLineAddedLength: Int,  // length WITHOUT moved ball
                          directionDetails: List[RelativeDirectionResult])

    def computeLineAxisResult(lineDirectionAxis: LineAxis): AxisResult = {
      println(s"+  computeLineAxisResult( axis = $lineDirectionAxis ).1")
      val directionsResults: List[RelativeDirectionResult] =
        relativeDirectionFactors.map { directionFactor =>
          computeDirectionResult(lineDirectionAxis,
                                 directionFactor)
        }
      val axisLineAddedLength = directionsResults.map(_.excursionLength).sum

      val result = AxisResult(axisLineAddedLength, directionsResults)  //????
      println(s"-  computeLineAxisResult( axis = $lineDirectionAxis ).9 result = $result")
      result
    }

    val newAxesResults: List[AxisResult] =
      lineAxes.map { lineAxis =>
        computeLineAxisResult(lineAxis)
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
        println("?????? CONTINUE: remove line(s) balls")
        Some(score)
    }
    println(s"-scoreMove(... ballTo = $ballTo...).9 = $result")
    result
  }

}
