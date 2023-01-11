package com.us.dsb.explore.algs.coloredlines.manual.game.lines

import com.us.dsb.explore.algs.coloredlines.manual.game.board.{BallKind, Board, BoardOrder, CellAddress, LineOrder, LowerGameState}
import com.us.dsb.explore.algs.coloredlines.manual.game.GameLogicSupport.BallArrivalResult

//???? TODO:  reduce repeated passing of board, ball color, etc.; maybe make
// LineDetector a class, to be instantiated for each move; or make local class
// for passing (but leave external-client interface same)
object LineDetector {

  private[lines] case class LineAxis(labelArray: String,
                                     rowDelta: Int, // -1 / 0 / 1 (Make refined type?)
                                     colDelta: Int)

  private[this] val lineAxes =
    List(
      LineAxis("→",  0, +1), // →  W -->  E
      LineAxis("↘", +1, +1), // ↘ NW --> SE
      LineAxis("↓", +1,  0), // ↓ N  --> S
      LineAxis("↙", +1, -1)) // ↙ NW --> SW

  private[this] val relativeDirectionFactors = List(1, -1) // use type of length 2 (refined List?, Tuple2?, some array?)

  private[lines] def haveMatchingBallAt(moveBallColor: BallKind,
                                        board: Board,
                                        rawRowIndex: Int,
                                        rawColIndex: Int): Boolean = {
    val inRange =
      1 <= rawRowIndex && rawRowIndex <= BoardOrder &&
          1 <= rawColIndex && rawColIndex <= BoardOrder
    val haveMatch =
      inRange && {
        val candidateAddress = CellAddress.fromRaw(rawRowIndex, rawColIndex)
        board.getBallStateAt(candidateAddress).fold(false)(ball => ball == moveBallColor)
      }
    haveMatch
  }

  private[lines] case class RelativeDirectionResult(excursionLength: Int)

  private[lines] def computeDirectionResult(moveBallColor: BallKind,
                                            board: Board,
                                            ballTo: CellAddress,
                                            lineDirectionAxis: LineAxis,
                                            directionFactor: Int): RelativeDirectionResult = {
    val newBallRowIndex = ballTo.row.value.value
    val newBallColIndex = ballTo.column.value.value
    import lineDirectionAxis.{colDelta, rowDelta}
    var excursionLength = 0
    while ( {
      val candidateExcursionLength = excursionLength + 1
      val candidateRowIndex = newBallRowIndex + rowDelta * directionFactor * candidateExcursionLength
      val candidateColIndex = newBallColIndex + colDelta * directionFactor * candidateExcursionLength

      val haveMatchingBall = haveMatchingBallAt(moveBallColor, board, candidateRowIndex, candidateColIndex)
      if (haveMatchingBall) {
        excursionLength = candidateExcursionLength
      }
      haveMatchingBall
    }) {}
    RelativeDirectionResult(excursionLength)
  }

  private[lines] case class AxisResult(axis: LineAxis,
                                       axisLineAddedLength: Int, // length WITHOUT moved ball
                                       directionDetails: List[RelativeDirectionResult])

  private[lines] def computeLineAxisResult(moveBallColor: BallKind,
                                           board: Board,
                                           ballTo: CellAddress,
                                           lineDirectionAxis: LineAxis): AxisResult = {
    val directionsResults: List[RelativeDirectionResult] =
      relativeDirectionFactors.map { directionFactor =>
        computeDirectionResult(moveBallColor,
                               board,
                               ballTo,
                               lineDirectionAxis,
                               directionFactor)
      }
    val axisLineAddedLength = directionsResults.map(_.excursionLength).sum
    AxisResult(lineDirectionAxis, axisLineAddedLength, directionsResults)
  }

  private[lines] def removeCompletedLineBalls(ballTo: CellAddress,
                                              preremovalGameState: LowerGameState,
                                              completedLineAxesResults: List[AxisResult]): LowerGameState = {
    val newBallRemovedGameState = preremovalGameState.withBoardWithNoBallAt(ballTo)
    val linesRemovedGameState =
      completedLineAxesResults.foldLeft(newBallRemovedGameState) { case (axisBoard, axisResult) =>
        val fromOffset = -axisResult.directionDetails(1).excursionLength
        val toOffset = axisResult.directionDetails(0).excursionLength
        val lineRemovedGameState =
          (fromOffset to toOffset).foldLeft(axisBoard) { case (directionBoard, offset) =>
            import axisResult.axis.{colDelta, rowDelta}
            val rawRowIndex = ballTo.row.value.value    + rowDelta * offset
            val rawColIndex = ballTo.column.value.value + colDelta * offset
            val cellAddress = CellAddress.fromRaw(rawRowIndex, rawColIndex)
            directionBoard.withBoardWithNoBallAt(cellAddress)
          }
        lineRemovedGameState
      }
    linesRemovedGameState
  }

  /** Reaps any complete lines from just-placed ball.
   * None if no line(s) completed; score increment otherwise
   */
  private[game] def reapAnyLines(gameState: LowerGameState,
                                 ballTo: CellAddress
                                ): BallArrivalResult = {
    //println(s"+handleBallArrival(... ballTo = $ballTo...).1")
    val moveBallColor = gameState.board.getBallStateAt(ballTo).get //????
    println(s"* * placed at $ballTo: $moveBallColor")

    val allAxesResults: List[AxisResult] =
      lineAxes.map { lineAxis =>
        computeLineAxisResult(moveBallColor, gameState.board, ballTo, lineAxis)
      }
    // println("? allAxesResults:" + allAxesResults.mkString("\n- ", "\n- ", ""))
    val completedLineAxesResults = allAxesResults.filter(_.axisLineAddedLength + 1 >= LineOrder)
    // println("? completedLineAxesResults:" + completedLineAxesResults.map("- " + _.toString).mkString("\n", "\n", "\n:end"))
    val (resultGameState, scoreResult) =
      completedLineAxesResults match {
        case Nil =>
          (gameState, None) // return None for score (signal to place 3 more IF ball moved by user)
        case linesAxes =>
          val totalBallsBeingRemoved = 1 + linesAxes.map(_.axisLineAddedLength).sum
          println(s"* * reaped at $ballTo: $totalBallsBeingRemoved $moveBallColor balls")
          //???? move?
          // note original game scoring: score = totalBallsBeingRemoved * 4 - 10,
          //  which seems to be from 2 pts per ball in 5-ball line, but 4 for any extra balls in line
          val postLinesRemovalGameState = removeCompletedLineBalls(ballTo,
                                                                   gameState,
                                                                   completedLineAxesResults)
          val ballPlacementScore = 2 * LineOrder + 4 * (totalBallsBeingRemoved - LineOrder)
          (postLinesRemovalGameState.withAddedScore(ballPlacementScore), Some(ballPlacementScore))
      }
    //println(s"-handleBallArrival(... ballTo = $ballTo...).9 = score result = $scoreResult")
    BallArrivalResult(resultGameState, anyRemovals = scoreResult.isDefined)
  }

}
