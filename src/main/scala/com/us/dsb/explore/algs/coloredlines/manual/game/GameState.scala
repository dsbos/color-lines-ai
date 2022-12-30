package com.us.dsb.explore.algs.coloredlines.manual.game

import cats.syntax.option._
import cats.syntax.either._
import com.us.dsb.explore.algs.coloredlines.manual.game.BallKind
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

    sealed trait OnBallOrEmpty
    case object OnBall  extends OnBallOrEmpty
    case object OnEmpty extends OnBallOrEmpty

    sealed trait OnSelOrUnsel
    case object OnSel   extends OnSelOrUnsel
    case object OnUnsel extends OnSelOrUnsel

    sealed trait HadBallOrNot
    case object HadBall extends HadBallOrNot
    case object NoBall  extends HadBallOrNot

    sealed trait HadSelOrNot
    case object HadSel extends HadSelOrNot
    case object NoSel   extends HadSelOrNot

    val onBallOrEmpty = if (board.hasABallAt(row, column))   OnBall  else OnEmpty
    val onSelOrUnsel  = if (board.isSelectedAt(row, column)) OnSel   else OnUnsel
    val hadBallOrNot  = if (board.hasABallSelected)          HadBall else NoBall
    val hadSelOrNot   = if (board.hasAnyCellSelected)        HadSel  else NoSel
    val action: Action = {
        val conditions = (onBallOrEmpty, hadBallOrNot, onSelOrUnsel, hadSelOrNot)
        conditions match {
          case (OnBall,  _,       OnUnsel, _     ) => SelectBall   // - tap on ball  when unselected
          case (OnEmpty, HadBall, _,       _     ) => TryMoveBall  // - tap on empty when ball selected

          case (OnEmpty, NoBall,  OnUnsel, NoSel ) => SelectEmpty  // - tap on empty when no selection
          case (OnEmpty, NoBall,  OnUnsel, HadSel) => Pass         // - tap on empty when selected

          case (_,       _,       OnSel,   _     ) => Deselect     // - tap on either when selected
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
