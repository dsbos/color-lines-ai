//??? TO BE reworked into tagless-final form:
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

  def callSimply(tio: SegregatedTextIO, dummy: String): IO[Either[String, UICommand]] = {
    for {
      rawCmd <- tio.readPromptedLine(s"Player $dummy command?: ")
      cmd = parseCommand(rawCmd)
      _ <- tio.printResult("Parsing result = " + cmd)
    } yield (cmd)
  }


  //@tailrec
  def getCommand(tio: SegregatedTextIO, dummy: String): IO[UICommand] = {
    for {
      rawCmd <- tio.readPromptedLine(s"Player $dummy command?: ")
      // ?? Q: Which form is more normal? (Note that error is re bad user input,
      //   no, say, IOException.):
      //cmdOrError <- IO(parseCommand(rawCmd))
      cmdOrError = parseCommand(rawCmd)
      eventualCmd <- cmdOrError match {
        case Right(cmd) =>
          // ?? Q:  This IO (along with the composed one for Left(...)) gets
          //   created only after things start running (since the parsed command
          //   can't be available before then).  Is it normal to create
          //   additional IO objects after things start running, or is something
          //   around here an abnormal way of doing things?
          IO.pure(cmd)
        case Left(msg) =>
          tio.printError(msg) *>  // ???? STUDY
          getCommand(tio, dummy) // loop
      }
    } yield eventualCmd

  }

  //java.lang.System.setProperty("cats.effect.stackTracingMode", "full")
  val callIo1 = callSimply(LiveColoredConsoleTextIO, "<whichever player>")
  println("callIo1 = " + callIo1)
  val result1 = callIo1.unsafeRunSync()
  println("result1 = " + result1)

  println()

  val result2 =
    getCommand(LiveColoredConsoleTextIO, "<whichever player>")
        .unsafeRunSync()
  println("result2 = " + result2)

}