package com.us.dsb.colorlines.game.logic

import com.us.dsb.colorlines.game.GameState
import com.us.dsb.colorlines.game.board.{BallColor, BoardReadView, CellAddress, Index}
import com.us.dsb.colorlines.game.lines.LineOrder

// ?? TODO:  reduce repeated passing of board, ball color, etc.:
//   - maybe make LineReaper a class, to be instantiated for each move
//   - maybe make local class for passing (but leave external-client interface same)
//   - maybe pass bundle class

/*??private[game]*/ object LineReaper {

  // ?? TODO:  Maybe make refined type for deltas? (check use w/relativeDirectionFactors):
  private case class LineAxis(labelArray: String,
                              rowDelta: Int,  // -1 / 0 / 1
                              colDelta: Int)

  private val lineAxes =
    List(
      LineAxis("→",  0, +1),  // →  W -->  E
      LineAxis("↘", +1, +1),  // ↘ NW --> SE
      LineAxis("↓", +1,  0),  // ↓ N  --> S
      LineAxis("↙", +1, -1))  // ↙ NW --> SW

  // ???? TODO:  Maybe use some iterable type of length 2  (refined List?, Tuple2?, some array?)
  private val relativeDirectionFactors = List(1, -1)

  // ([logic] for testing)
  private[logic] def haveMatchingBallAt(moveBallColor: BallColor,
                                        board: BoardReadView,
                                        rawRowIndex: Int,
                                        rawColIndex: Int): Boolean = {
    val inRange =
      Index.MinValue.value <= rawRowIndex && rawRowIndex <= Index.MaxValue.value &&
          Index.MinValue.value <= rawColIndex && rawColIndex <= Index.MaxValue.value
    val haveMatch =
      inRange && {
        val candidateAddress = CellAddress.fromRaw(rawRowIndex, rawColIndex)
        board.getCellStateAt(candidateAddress).asOption.fold(false)(ball => ball == moveBallColor)
      }
    haveMatch
  }

  // ?? TODO:  Would "spur" be better than long "excursion"?
  // ?? TODO:  Maybe flatten by eliminating DirectionExecursionLength:
  private case class DirectionExecursionLength(value: Int) extends AnyVal

  // ????? TODO:  Probably get ball color via ballTo, rather than having clients
  //   pass it down several levels:
  private def computeDirectionExecursionLength(moveBallColor: BallColor,
                                               board: BoardReadView,
                                               ballTo: CellAddress,
                                               lineDirectionAxis: LineAxis,
                                               lineDirectionFactor: Int
                                                ): DirectionExecursionLength = {
    // ???? TODO: Revisit names (shorten, to shorten lines)?
    val newBallRowIndex = ballTo.row.raw.value
    val newBallColIndex = ballTo.column.raw.value
    import lineDirectionAxis.{colDelta, rowDelta}
    var excursionLength = 0
    while ({
      val candidateExcursionLength = excursionLength + 1
      val candidateRowIndex = newBallRowIndex + rowDelta * lineDirectionFactor * candidateExcursionLength
      val candidateColIndex = newBallColIndex + colDelta * lineDirectionFactor * candidateExcursionLength

      val haveMatchingBall = haveMatchingBallAt(moveBallColor, board, candidateRowIndex, candidateColIndex)
      if (haveMatchingBall) {
        excursionLength = candidateExcursionLength
      }
      haveMatchingBall
    }) {}
    DirectionExecursionLength(excursionLength)
  }

  /**
   * @param axis
   *   for which axis
   * @param axisLineAddedLength
   *   length without placed ball
   * @param directionsResults
   *   axis subdirection results (execursion length; returned for removal code)
   */
  private case class AxisResult(axis: LineAxis,
                                axisLineAddedLength: Int,
                                directionExcursionLengths: List[DirectionExecursionLength])

  // ?????? TODO:  Would having lineDirectionAxis first be clearer/more logical?
  private def computeLineAxisResult(moveBallColor: BallColor,
                                    board: BoardReadView,
                                    ballTo: CellAddress,
                                    lineDirectionAxis: LineAxis
                                   ): AxisResult = {
    // ?? TODO:  maybe "...direction...lengths"?
    val directionExecursionLengths: List[DirectionExecursionLength] =
      relativeDirectionFactors.map { lineDirectionFactor =>
        computeDirectionExecursionLength(moveBallColor,
                                         board,
                                         ballTo,
                                         lineDirectionAxis,
                                         lineDirectionFactor)
      }
    val axisLineAddedLength = directionExecursionLengths.map(_.value).sum
    AxisResult(lineDirectionAxis, axisLineAddedLength, directionExecursionLengths)
  }

  /** Removes completed lines' balls. */
  private def removeCompletedLinesBalls(ballTo: CellAddress,
                                        preremovalGameState: GameState,
                                        completedLineAxesResults: List[AxisResult]
                                       ): GameState = {
    val newBallRemovedGameState = preremovalGameState.withBoardWithNoBallAt(ballTo)
    val linesRemovedGameState =
      completedLineAxesResults.foldLeft(newBallRemovedGameState) { (axisBoard, axisResult) =>
        val fromOffset = -axisResult.directionExcursionLengths(1).value
        val toOffset   =  axisResult.directionExcursionLengths(0).value
        val lineRemovedGameState =
          (fromOffset to toOffset).foldLeft(axisBoard) { (directionBoard, offset) =>
            import axisResult.axis.{colDelta, rowDelta}
            val rawRowIndex = ballTo.row.raw.value    + rowDelta * offset
            val rawColIndex = ballTo.column.raw.value + colDelta * offset
            val cellAddress = CellAddress.fromRaw(rawRowIndex, rawColIndex)
            directionBoard.withBoardWithNoBallAt(cellAddress)
          }
        lineRemovedGameState
      }
    linesRemovedGameState
  }

  private def computeReapingScore(numberOfBallsRemoved: Int): Int = {
    // Original game scoring was score = <number of balls removed> * 4 - 10,
    //  which seems to be from 2 points per ball in 5-ball line, but 4 each for
    //  any balls in line beyond 5.
    2 * LineOrder + 4 * (numberOfBallsRemoved - LineOrder)
  }

  /**
   * @param gameState
   *   updated game state
   * @param anyRemovals
   *   whether any lines reaped
   */
  private[game] case class ReapingAttemptResult(gameState: GameState,
                                                anyRemovals: Boolean)
  /** Reaps any complete lines from just-placed ball.
   * @return
   *   Updated board and score if any completed lines; input state if no lines.
   */
  /*??private[game]*/ def reapAnyLines(gameState: GameState,
                                 ballTo: CellAddress
                                ): ReapingAttemptResult = {
    // ????? TODO:  Resolve following .get.
    //   - Maybe use getOrElse with "Unexpected case".
    //   - Maybe add BallColor parameter (pros and cans ...).
    //   - Maybe eliminate passing BallColor down, having deeper method (re-)get
    //     it via ballTo and board.
    val moveBallColor = gameState.board.getCellStateAt(ballTo).asOption.get //?????? pass in?  getOrElse?
    println(s"* * placed at $ballTo: $moveBallColor")

    val allAxesResults: List[AxisResult] =
      lineAxes.map { lineAxis =>
        computeLineAxisResult(moveBallColor, gameState.board, ballTo, lineAxis)
      }
    val completedLineAxesResults =
      allAxesResults.filter(_.axisLineAddedLength + 1 >= LineOrder)
    val (resultGameState, scoreResult) =
      completedLineAxesResults match {
        case Nil =>
          (gameState, None) // None for score leads to placing on-deck balls
        case axesResults =>
          val totalBallsBeingRemoved = 1 + axesResults.map(_.axisLineAddedLength).sum
          println(s"* * reaped at $ballTo: $totalBallsBeingRemoved $moveBallColor balls")
          val postLinesRemovalGameState =
            removeCompletedLinesBalls(ballTo, gameState, completedLineAxesResults)
          val ballPlacementScore = computeReapingScore(totalBallsBeingRemoved)
          (postLinesRemovalGameState.withAddedScore(ballPlacementScore), Some(ballPlacementScore))
      }
    ReapingAttemptResult(resultGameState, anyRemovals = scoreResult.isDefined)
  }

}
