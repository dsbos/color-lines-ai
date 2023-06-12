package com.us.dsb.explore.algs.coloredlines.manual.game.lines

import com.us.dsb.explore.algs.coloredlines.manual.game.board.{
  BallColor, Board, BoardOrder, CellAddress, LineOrder, LowerGameState}

// ?? TODO:  reduce repeated passing of board, ball color, etc.; maybe make
// LineDetector a class, to be instantiated for each move; or make local class
// for passing (but leave external-client interface same)
// ?? TODO Revisit containment of BallArrivalResult inside; that's the reason
//   LineDetector's "private[game]" was widened to "private[manual]"
private[manual] object LineDetector {

  // ?? TODO:  Maybe make refined type for deltas? (check use w/relativeDirectionFactors):
  private[lines] case class LineAxis(labelArray: String,
                                     rowDelta: Int, // -1 / 0 / 1
                                     colDelta: Int)

  private[this] val lineAxes =
    List(
      LineAxis("→",  0, +1), // →  W -->  E
      LineAxis("↘", +1, +1), // ↘ NW --> SE
      LineAxis("↓", +1,  0), // ↓ N  --> S
      LineAxis("↙", +1, -1)) // ↙ NW --> SW

  private[this] val relativeDirectionFactors = List(1, -1) // use type of length 2 (refined List?, Tuple2?, some array?)

  private[lines] def haveMatchingBallAt(moveBallColor: BallColor,
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

  // ???? TODO:  Review names ("result" -> "...length..."?); maybe newtype (to flatten)?

  private[lines] case class RelativeDirectionResult(excursionLength: Int)

  private[lines] def computeDirectionResult(moveBallColor: BallColor,
                                            board: Board,
                                            ballTo: CellAddress,
                                            lineDirectionAxis: LineAxis,
                                            lineDirectionFactor: Int): RelativeDirectionResult = {
    // ???? TODO: Revisit names (shorten, to shorten lines)?
    val newBallRowIndex = ballTo.row.value.value
    val newBallColIndex = ballTo.column.value.value
    import lineDirectionAxis.{colDelta, rowDelta}
    var excursionLength = 0
    while ( {
      val candidateExcursionLength = excursionLength + 1
      val candidateRowIndex = newBallRowIndex + rowDelta * lineDirectionFactor * candidateExcursionLength
      val candidateColIndex = newBallColIndex + colDelta * lineDirectionFactor * candidateExcursionLength

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
                                       // ?? TODO:  maybe "...direction...lengths"?:
                                       directionsResults: List[RelativeDirectionResult])

  private[lines] def computeLineAxisResult(moveBallColor: BallColor,
                                           board: Board,
                                           ballTo: CellAddress,
                                           lineDirectionAxis: LineAxis): AxisResult = {
    // ?? TODO:  maybe "...direction...lengths"?
    val directionsResults: List[RelativeDirectionResult] =
      relativeDirectionFactors.map { lineDirectionFactor =>
        computeDirectionResult(moveBallColor,
                               board,
                               ballTo,
                               lineDirectionAxis,
                               lineDirectionFactor)
      }
    val axisLineAddedLength = directionsResults.map(_.excursionLength).sum
    AxisResult(lineDirectionAxis, axisLineAddedLength, directionsResults)
  }

  /** Removes completed lines' balls. */
  private[lines] def removeCompletedLinesBalls(ballTo: CellAddress,
                                               preremovalGameState: LowerGameState,
                                               completedLineAxesResults: List[AxisResult]): LowerGameState = {
    val newBallRemovedGameState = preremovalGameState.withBoardWithNoBallAt(ballTo)
    val linesRemovedGameState =
      completedLineAxesResults.foldLeft(newBallRemovedGameState) { case (axisBoard, axisResult) =>
        val fromOffset = -axisResult.directionsResults(1).excursionLength
        val toOffset   =  axisResult.directionsResults(0).excursionLength
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

  // ?????? TODO:  Review uses (return type):  Which one don't need to report whether any removals?
  /**
   * @param gameState
   *   updated game state
   * @param anyRemovals
   *   whether any lines reaped (re placing on-deck balls)
   */
  private[manual] case class BallArrivalResult(gameState: LowerGameState,
                                             anyRemovals: Boolean)
  {
    println(s"* $this")
  }

  /** Reaps any complete lines from just-placed ball.
   * @return
   *   Updated board and score if any completed lines; input state if no lines.
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
    val completedLineAxesResults =
      allAxesResults.filter(_.axisLineAddedLength + 1 >= LineOrder)
    // println("? completedLineAxesResults:" + completedLineAxesResults.map("- " + _.toString).mkString("\n", "\n", "\n:end"))
    val (resultGameState, scoreResult) =
      completedLineAxesResults match {
        case Nil =>
          (gameState, None) // return None for score (signal to place 3 more IF ball moved by user)
        case linesAxes =>
          //????? test
          val totalBallsBeingRemoved = 1 + linesAxes.map(_.axisLineAddedLength).sum
          println(s"* * reaped at $ballTo: $totalBallsBeingRemoved $moveBallColor balls")
          //???? move?
          // note original game scoring: score = totalBallsBeingRemoved * 4 - 10,
          //  which seems to be from 2 pts per ball in 5-ball line, but 4 for any extra balls in line
          val postLinesRemovalGameState = removeCompletedLinesBalls(ballTo,
                                                                    gameState,
                                                                    completedLineAxesResults)
          // ???? TODO:  Pull out method to make score function clearer.
          val ballPlacementScore = 2 * LineOrder + 4 * (totalBallsBeingRemoved - LineOrder)
          (postLinesRemovalGameState.withAddedScore(ballPlacementScore), Some(ballPlacementScore))
      }
    //println(s"-handleBallArrival(... ballTo = $ballTo...).9 = score result = $scoreResult")
    BallArrivalResult(resultGameState, anyRemovals = scoreResult.isDefined)
  }

}
