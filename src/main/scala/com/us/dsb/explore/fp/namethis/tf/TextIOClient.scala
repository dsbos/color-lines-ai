package com.us.dsb.explore.fp.namethis.tf

import cats.effect.IO
import com.us.dsb.explore.fp.namethis.tf.LiveColoredConsoleTextIO
import cats.syntax.either._


/** TTT UI controller. */
object TextIOClient extends App {

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

  private def callSimply(tio: SegregatedTextIO, dummy: String): IO[Either[String, UICommand]] = {
    for {
      rawCmd <- tio.readPromptedLine(s"Player $dummy command?: ")
    } yield (parseCommand(rawCmd))
  }


  //@tailrec
  private def getCommand(tio: SegregatedTextIO, dummy: String): IO[UICommand] = {
    val cmdOrErrorIO =
      for {
        rawCmd <- tio.readPromptedLine(s"Player $dummy command?: ")
        cmdOrError <- IO(parseCommand(rawCmd))
      } yield {
        cmdOrError
      }
    for {
      cmdOrError <- cmdOrErrorIO
      eventualCmd <- cmdOrError match {
        case Right(cmd) => IO(cmd)
        case Left(msg) =>
          tio.printError(msg)  // ???? doesn't print, since printError used IO; how do I execute it?  for/flatMap?
          println("*** Q:  How to execute printerror(...): IO[Unit]?")
          getCommand(tio, dummy) // loop
      }
    } yield eventualCmd

  }

  val result1 =
    callSimply(LiveColoredConsoleTextIO, "<whichever player>")
        .unsafeRunSync()
  println("result1 = " + result1)

  println()

  val result2 =
    getCommand(LiveColoredConsoleTextIO, "<whichever player>")
        .unsafeRunSync()
  println("result2 = " + result2)

}