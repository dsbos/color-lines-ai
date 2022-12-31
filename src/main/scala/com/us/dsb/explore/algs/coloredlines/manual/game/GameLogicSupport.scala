package com.us.dsb.explore.algs.coloredlines.manual.game

import scala.annotation.tailrec
import scala.util.Random

object GameLogicSupport {

  private[this] def pickRandomBallKind(rng: Random): BallKind = BallKind.values(rng.nextInt(BallKind.values.size))

  @tailrec
  private[this] def pickRandomEmptyCell(rng: Random, board: Board): Option[(RowIndex, ColumnIndex)] = {
    if (board.isFull)
      None
    else {
      val row = rowIndices(rng.nextInt(BoardOrder))
      val col = columnIndices(rng.nextInt(BoardOrder))
      if (board.getBallStateAt(row, col).isEmpty)
        Some((row, col))
      else
        pickRandomEmptyCell(rng, board) // loop: try again
    }
  }

  /**
   * @param board
   *   expected to be empty //???? maybe refactor something?
   */
  private[game] def placeInitialBalls(rng: Random, board: Board): Board = {
    val newBoard1 =
      (1 to 5).foldLeft(board) { //???? parameterize
        case (curBoard, _) =>  //???? refactor?
          val (row, column) =
            pickRandomEmptyCell(rng, curBoard)
                .getOrElse(scala.sys.error("Unexpectedly full board"))
          curBoard.withCellHavingBall(row, column, pickRandomBallKind(rng))
      }
    newBoard1.withOnDeckBalls( List.fill(3)(pickRandomBallKind(rng)))
  }

  private[game] sealed trait Action
  private[game] object Action {
    private[game] case object SelectBall  extends Action
    private[game] case object SelectEmpty extends Action
    private[game] case object Deselect    extends Action
    private[game] case object TryMoveBall extends Action
    private[game] case object Pass        extends Action
  }
  import Action._

  def interpretTapLocationToTapAction(board: Board,
                                      row: RowIndex,
                                      column: ColumnIndex): Action =
    tapAndStateToTapAction(onABall            = board.hasABallAt(row, column),
                           isSelectedAt       = board.isSelectedAt(row, column),
                           hasABallSelected   = board.hasABallSelected,
                           hasAnyCellSelected = board.hasAnyCellSelected)

  private def tapAndStateToTapAction(onABall: Boolean,
                                     isSelectedAt: Boolean,
                                     hasABallSelected: Boolean,
                                     hasAnyCellSelected: Boolean
                                    ): Action = {
    object RenameOrFlattenThis { // grouped/nested re debugger clutter
      sealed trait OnBallOrEmpty
      case object OnBall extends OnBallOrEmpty
      case object OnEmpty extends OnBallOrEmpty

      sealed trait OnSelOrUnsel
      case object OnSel extends OnSelOrUnsel
      case object OnUnsel extends OnSelOrUnsel

      sealed trait HadBallOrNot
      case object HadBall extends HadBallOrNot
      case object NoBall extends HadBallOrNot

      sealed trait HadSelOrNot
      case object HadSel extends HadSelOrNot
      case object NoSel extends HadSelOrNot
    }
    import RenameOrFlattenThis._

    val onBallOrEmpty = if (onABall)            OnBall  else OnEmpty
    val onSelOrUnsel  = if (isSelectedAt)       OnSel   else OnUnsel
    val hadBallOrNot  = if (hasABallSelected)   HadBall else NoBall
    val hadSelOrNot   = if (hasAnyCellSelected) HadSel  else NoSel

    val action: Action = {
      val conditions = (onBallOrEmpty, hadBallOrNot, onSelOrUnsel, hadSelOrNot)
      conditions match {
        case (OnBall,  _,       OnUnsel, _     ) => SelectBall  // - tap on ball  when unselected
        case (OnEmpty, HadBall, _,       _     ) => TryMoveBall // - tap on empty when ball selected

        case (OnEmpty, NoBall,  OnUnsel, NoSel ) => SelectEmpty // - tap on empty when no selection
        case (OnEmpty, NoBall,  OnUnsel, HadSel) => Pass        // - tap on empty when selected

        case (_,       _,       OnSel,   _     ) => Deselect    // - tap on either when selected
      }
    }
    action
  }

  private[this] def placeNextBalls(rng: Random, board: Board): Board = {
    val newBoard =
      board.getOnDeckBalls
        .foldLeft(board) {
          case (curBoard, _) =>
            pickRandomEmptyCell(rng, curBoard) match {
              case None => curBoard
              case Some((row, column)) =>
                curBoard.withCellHavingBall(row, column, pickRandomBallKind(rng))
            }
        }
    newBoard.withOnDeckBalls(List.fill(3)(pickRandomBallKind(rng)))
  }

  //????? decide where movability check is called and how/where tap state is udpated
  case class MoveResult(newBoard: Board, addedScore: Option[Int])

  private[game] def doPass(rng: Random, board: Board): MoveResult =
    MoveResult(placeNextBalls(rng, board), None)


  private[game] def doTryMoveBall(rng: Random,
                                  board: Board,  //???? change to game state to carry, update score
                                  fromRow: RowIndex,
                                  fromCol: ColumnIndex, //???? combine into cell coord. pair/ID
                                  toRow: RowIndex,
                                  toCol: ColumnIndex
                                  ): MoveResult = {
    val canMoveBall = rng.nextBoolean()
    println("NIY: doMoveBall; doing RANDOM: canMoveBall " + canMoveBall)

    canMoveBall match {
      case false =>  // can't move--ignore (keep selection state)
        MoveResult(board, None)
      case true =>
        val completesAnyLines = rng.nextBoolean()
        println("-                              completesAnyLines " + completesAnyLines)
        completesAnyLines match {
          case false =>
            //?????? place next three
            MoveResult(placeNextBalls(rng, board), None) //???? clear selection state
          case true =>
            val ballCount = rng.between(5, 10)
            println("-                              ballCount " + ballCount)
            val moveScore = ballCount * 4 - 10
            //?????? "harvest" lines (clear, score)
            MoveResult(placeNextBalls(rng, board), Some(moveScore)) //???? clear selection state
        }
    }
  }

}

