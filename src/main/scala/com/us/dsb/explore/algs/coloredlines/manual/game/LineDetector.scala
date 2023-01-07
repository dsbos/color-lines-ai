package com.us.dsb.explore.algs.coloredlines.manual.game

import com.us.dsb.explore.algs.coloredlines.manual.game.Board.CellAddress

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

//???? TODO:  reduce repeated passing of board, etc.; maybe make LineDetector a
// class, to be instantiated for each move; or make local class for passing (but
// leave external-client inteface same
object LineDetector {

  private[this] case class LineAxis(labelArray: String,
                                    rowDelta: Int, // -1 / 0 / 1 (Make refined type?)
                                    colDelta: Int)
  private[this] val lineAxes =
    List(
      LineAxis("→",  0, +1),  // →  W -->  E
      LineAxis("↘", +1, +1),  // ↘ NW --> SE
      LineAxis("↓", +1,  0),  // ↓ N  --> S
      LineAxis("↙", +1, -1))  // ↙ NW --> SW
  private[this] val relativeDirectionFactors = List(1, -1) // use type of length 2 (refined List?, Tuple2?, some array?)

  private[this] def haveMatchingBallAt(moveBallColor: BallKind,
                                       board: Board,
                                       rawRowIndex: Int,
                                       rawColIndex: Int): Boolean = {
    val inRange =
      1   <= rawRowIndex && rawRowIndex <= BoardOrder &&
        1 <= rawColIndex && rawColIndex <= BoardOrder
    val haveMatch =
      inRange && {
        val candidateAddress = CellAddress(RowIndex(Index.unsafeFrom(rawRowIndex)),
                                           ColumnIndex(Index.unsafeFrom(rawColIndex)))
        board.getBallStateAt(candidateAddress).fold(false)(ball => ball == moveBallColor)
      }
    haveMatch
  }

  private[this] case class RelativeDirectionResult(excursionLength: Int)

  private[this] def computeDirectionResult(moveBallColor: BallKind,
                                           board: Board,
                                           ballTo: CellAddress,
                                           lineDirectionAxis: LineAxis,
                                           directionFactor: Int): RelativeDirectionResult = {
    val newBallRowIndex = ballTo.row.value.value
    val newBallColIndex = ballTo.column.value.value
    import lineDirectionAxis.{rowDelta, colDelta}
    var excursionLength = 0
    while ( {
      val candidateExcursionLength = excursionLength + 1
      val candidateRowIndex = newBallRowIndex + rowDelta * directionFactor * candidateExcursionLength
      val candidateColIndex = newBallColIndex + colDelta * directionFactor * candidateExcursionLength
      println(s"    ??.n.0: candidate address: ($candidateRowIndex / $candidateColIndex)")

      val haveMatchingBall = haveMatchingBallAt(moveBallColor, board, candidateRowIndex, candidateColIndex)
      if (haveMatchingBall) {
        excursionLength = candidateExcursionLength
      }
      haveMatchingBall
    }) {}
    RelativeDirectionResult(excursionLength)
  }

  //??? maybe save axis vector (for use in ball deletion)
  private[this] case class AxisResult(axis: LineAxis,
                                      axisLineAddedLength: Int,  // length WITHOUT moved ball
                                      directionDetails: List[RelativeDirectionResult])

  private[this] def computeLineAxisResult(moveBallColor: BallKind,
                                          board: Board,
                                          ballTo: CellAddress,
                                          lineDirectionAxis: LineAxis): AxisResult = {
    println(s"+  computeLineAxisResult( axis = $lineDirectionAxis ).1")
    val directionsResults: List[RelativeDirectionResult] =
      relativeDirectionFactors.map { directionFactor =>
        computeDirectionResult(moveBallColor,
                               board,
                               ballTo,
                               lineDirectionAxis,
                               directionFactor)
      }
    val axisLineAddedLength = directionsResults.map(_.excursionLength).sum

    val result = AxisResult(lineDirectionAxis, axisLineAddedLength, directionsResults)  //????
    println(s"-  computeLineAxisResult( axis = $lineDirectionAxis ).9 result = $result")
    result
  }

  /**
   * @return
   *   None if no line(s) completed; score increment otherwise
   */
  private[game] def handleBallArrival(board: Board,
                                      ballFrom: CellAddress,
                                      ballTo: CellAddress
                                     ): (Board, Option[Int]) = {
    println(s"+handleBallArrival(... ballTo = $ballTo...).1")
    val moveBallColor = board.getBallStateAt(ballTo).get //????

    val allAxesResults: List[AxisResult] =
      lineAxes.map { lineAxis =>
        computeLineAxisResult(moveBallColor, board, ballTo, lineAxis)
      }
    println("??? allAxesResults:" + allAxesResults.mkString("\n- ", "\n- ", ""))
    val completedLineAxesResults = allAxesResults.filter(_.axisLineAddedLength + 1 >= LineOrder)
    println("??? completedLineAxesResults:" + completedLineAxesResults.mkString("\n- ", "\n- ", ""))
    val (boardResult, scoreResult) =
      completedLineAxesResults match {
        case Nil =>
          (board, None) // return None for score (signal to place 3 more IF ball moved by user)
        case linesAxes =>
          val totalBallsBeingRemoved = 1 + linesAxes.map(_.axisLineAddedLength).sum
          println(s" scoreMove(... ballTo = $ballTo...).x totalBallsBeingRemoved = $totalBallsBeingRemoved")
          val score = totalBallsBeingRemoved * 4 - 10

          def removeCompletedLineBalls(preremovalBoard: Board,
                                       completedLineAxesResults: List[AxisResult]): Board = {
            val newBallRemovedBoard = preremovalBoard.withCellHavingNoBall(ballTo)
            val linesRemovedBoard =
              completedLineAxesResults.foldLeft(newBallRemovedBoard){ case (axisBoard, axisData) =>
                val fromOffset = - axisData.directionDetails(1).excursionLength
                val toOffset   = axisData.directionDetails(0).excursionLength
                val lineRemovedBoard =
                  (fromOffset to toOffset).foldLeft(axisBoard) { case (directionBoard, xxoffset) =>
                    val xxnewBallRowIndex = ballTo.row.value.value
                    val xxnewBallColIndex = ballTo.column.value.value
                    import axisData.axis.{rowDelta, colDelta}
                    val rawRowIndex = xxnewBallRowIndex + rowDelta * xxoffset
                    val rawColIndex = xxnewBallColIndex + colDelta * xxoffset
                    val cellAddress = CellAddress(RowIndex(Index.unsafeFrom(rawRowIndex)),
                                                 ColumnIndex(Index.unsafeFrom(rawColIndex)))
                    directionBoard.withCellHavingNoBall(cellAddress)
                  }
                lineRemovedBoard
              }
            linesRemovedBoard
          }

          val postLinesRemovalBoard = removeCompletedLineBalls(board,
                                                               completedLineAxesResults);
          (postLinesRemovalBoard, Some(score))
      }
    println(s"-handleBallArrival(... ballTo = $ballTo...).9 = score result = $scoreResult")
    (boardResult, scoreResult)
  }

}
