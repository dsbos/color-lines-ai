package com.us.dsb.explore.fp.namethis.orig

import cats.syntax.option._
import cats.syntax.either._
//import com.us.dsb.explore.fp.namethis.game.{ColumnIndex, GameState, Index, Player, RowIndex}
import enumeratum.{Enum, EnumEntry}

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

/** TTT UI controller. */
object TextIOClient {

  // ("extends EnumEntry" gets .entryName, enables Enum; "extends Enum[...]"
  // enables (and requires) .values.

  sealed trait UICommand
  object UICommand {
    // (Q: Why doesn't UICommand's "sealed" obviate the following one (for
    //   exhaustive-match checks)?
    sealed trait UIMoveCommand extends UICommand
    case object Up    extends UIMoveCommand
    case object Down  extends UIMoveCommand
    case object Left  extends UIMoveCommand
    case object Right extends UIMoveCommand
    case object Mark  extends UICommand
    case object Quit  extends UICommand
  }
  // ?? Decide "UICommand._" re little scala.Right ~clashes.

  // (Could put strings in enumerators and use Enum.withName to factor down
  // parse function, but then (sub)layers wouldn't be separated.)

  // ?? revisit Either--use something fancier (MonadError)?
  private def parseCommand(rawCmd: String): Either[String, UICommand] = {
    import UICommand._
    rawCmd match {
      case "u" => Up.asRight
      case "d" => Down.asRight
      case "l" => Left.asRight
      case "r" => Right.asRight
      case "m" => Mark.asRight
      case "q" => Quit.asRight
      case _ =>
        s"Invalid input \"$rawCmd\"; try u(p), d(own), l(eft), r(right), m(ark), or q(uit)".asLeft
    }
  }

  private def callSimply(io: SegregatedTextIO, dummy: String): Either[String, UICommand] = {
    val rawCmd = io.readPromptedLine(s"Player $dummy command?: ")
    parseCommand(rawCmd)
  }

  @tailrec
  private def getCommand(io: SegregatedTextIO, dummy: String): UICommand = {
    val rawCmd = io.readPromptedLine(s"Player $dummy command?: ")
    parseCommand(rawCmd) match {
      case Right(cmd) => cmd
      case Left(msg) =>
        io.printError(msg)
        getCommand(io, dummy)  // loop
    }
  }


}