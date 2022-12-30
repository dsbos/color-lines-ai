package com.us.dsb.explore.algs.coloredlines.manual.game

import cats.syntax.option._
import cats.syntax.either._
import com.us.dsb.explore.algs.coloredlines.manual.game.Board.BallKind
import com.us.dsb.explore.algs.coloredlines.manual.game.XxGameState.XxGameResult.XxxPlaceholderDone
import com.us.dsb.explore.algs.coloredlines.manual.game.{ColumnIndex, RowIndex}

import scala.util.Random


private[manual] object XxGameState {

  /**
   * Result of completed game.
   */
  sealed trait XxxGameResult //???? change to final score (ane maybe stats?)
  object XxGameResult {
    case class XxxPlaceholderDone(stats: Float) extends XxxGameResult
  }

  def xxselectRandomEmptyCell(rng: Random, board: Board): (RowIndex, ColumnIndex) = {
    val row = rowIndices(rng.nextInt(BoardOrder))
    val col = columnIndices(rng.nextInt(BoardOrder))
    if (board.getBallStateAt(row, col).isEmpty)
      (row, col)
    else
      xxselectRandomEmptyCell(rng, board) // loop: try again
  }

  private def xxmakeInitialState(rng: Random): XxGameState = {
    def getRandomBallKind() = BallKind.values(rng.nextInt(BallKind.values.size))

    val board1 = Board.empty

    val board3 =
      (1 to 5).foldLeft(board1) {
        case (board2, _) =>
          val (row, column) = xxselectRandomEmptyCell(rng, board2)
          board2.withCellHavingBall(row, column, getRandomBallKind())
      }

    //??? add 3 on-back balls


    XxGameState(rng, board3, None)
  }

  def xxinitial(seed: Long): XxGameState = xxmakeInitialState(new Random(seed))
  def xxinitial: XxGameState = xxmakeInitialState(new Random())
}
import XxGameState._

/**
 * TTT game state _and_ controller--should functions be separated or together?
 *
 * @param gameResult  `None` means no win or draw yet
 */
 //???? add random-data state


/** Game state AND currently controller.
 */
private[manual] case class XxGameState(rng: Random,
                                       board: Board,
                                       gameResult: Option[XxxGameResult]
                                      ) {

  def xxpickRandomEmptyCell(): Option[(RowIndex, ColumnIndex)] = {
    if (board.isFull)
      None
    else {
      val row = rowIndices(rng.nextInt(BoardOrder))
      val col = columnIndices(rng.nextInt(BoardOrder))
      if (board.getBallStateAt(row, col).isEmpty)
        Some((row, col))
      else
        xxpickRandomEmptyCell() // loop: try again
    }
  }

  def xxgetRandomBallKind() = BallKind.values(rng.nextInt(BallKind.values.size))

  def xxdoPass(): Board = {
    // ???? scatter 3 on-deck balls around board (unless no more room)
    val onDeckListPlaceholder = List.fill(3)(xxgetRandomBallKind())
    val newBoard =
      onDeckListPlaceholder
        .foldLeft(board) {
          case (board2, _) =>
            xxpickRandomEmptyCell match {
              case None => board2
              case Some((row, column)) => board2.withCellHavingBall(row, column, xxgetRandomBallKind())
            }
        }
    newBoard
  }


  // ?? later refine from Either[String, ...] to "fancier" error type
  // Xx?? maybe add result of move (win/draw/other) with new state (so caller
  //  doesn't have to check state's gameResult; also, think about where I'd add
  //  game history

  def xxtryMoveAt(row: RowIndex,
                column: ColumnIndex
               ): Either[String, XxGameState] = {
    trait Action
    object Action {
      case object SelectBall  extends Action
      case object SelectEmpty extends Action
      case object Deselect    extends Action
      case object TryMoveBall extends Action
      case object Pass        extends Action
    }
    import Action._

    val onBall  = board.hasABallAt(row, column)
    val hadBall = board.hasABallSelected
    val hadSel  = board.hasACellSelected
    val onSel   = board.getSelectionStateAt(row, column)
    val conditions1 = Tuple4(onBall, hadBall, onSel, hadSel)
//    println("conditions = " +
//                conditions.productIterator.map(x => x.asInstanceOf[Boolean]).map(if (_) "t" else "f").mkString(","))
    val action: Action = {
      if (onBall) {
        // Tap on ball--select it, except clear if it was selected:
        if (! onSel) SelectBall else Deselect
      }
      else {
        // Top on empty cell:
        val conditions2 = (hadBall, onSel, hadSel)
        //??? onSel  implies hadSel
        //??? hadBal implies hasSel
        conditions2 match {
          case (true,  true,  _    ) => ???          // - can't be on empty AND on ball that was selected
          case (true,  false, true ) => TryMoveBall  // - had ball, now tap on empty
          case (true,  false, false) => ???          // - can't have a selected ball AND no selection

          case (false, true,  _    ) => Deselect     // - had empty, now tap on _same_ empty
          case (false, false, true ) => Pass         // - had empty, tap on different empty
          case (false, false, false) => SelectEmpty  // - had no selection, tap on emptuy
        }
      }

    }
    val nextBoard =
      action match {
        case SelectBall |
             SelectEmpty => board.withCellSelected(row, column)
        case Deselect    => board.withNoSelection
        case TryMoveBall =>
          //?????? do ... path check, ball update, on deck, etc. around here
          println("NIY: " + action); board.withNoSelection
        case Pass        => xxdoPass.withNoSelection  //??? clean board ref

      }

    val nextState =
      if (! nextBoard.isFull) {
        XxGameState(rng, nextBoard, gameResult).asRight
      }
      else {
        XxGameState(rng, nextBoard, Some(XxxPlaceholderDone(1.23f))).asRight
      }
    nextState
  }

}
