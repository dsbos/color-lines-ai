//??? BEING REWORKED tagless-final(?) form:
package com.us.dsb.explore.fp.namethis.tf

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

  import cats.{Applicative, Apply, FlatMap, Functor, Monad}

  // (FlatMap includes Functor:)
  def callSimply[F[_]: FlatMap](tio: SegregatedTextIO[F], dummy: String): F[Either[String, UICommand]] = {
    import cats.syntax.functor._  // to wrap "x <- ...: F" to get .map
    import cats.syntax.flatMap._  // to wrap "x <- ...: F" to get .flatMap
    for {
      rawCmd <- tio.readPromptedLine(s"Player $dummy command?: ")
      cmd = parseCommand(rawCmd)
      _ <- tio.printResult("Parsing result = " + cmd)
    } yield (cmd)
  }

  //@tailrec
  // (Monad includes FlatMap, Applicative, etc.:
  def getCommand[F[_]: Monad](tio: SegregatedTextIO[F], dummy: String): F[Either[String, UICommand]] = {
    import cats.syntax.functor._
    import cats.syntax.flatMap._
    import cats.syntax.applicative._
    import cats.syntax.apply._
    //import cats.syntax.all._
    for {
      // getting "value map is not a member of type parameter F[String]" here:
      //rawCmd <- tio.readPromptedLine(s"Player $dummy command?: ")
      rawCmd <- tio.readPromptedLine(s"Player $dummy command?: ")
      cmdOrError = parseCommand(rawCmd)
      _ <- tio.printResult("Parsing result = " + cmdOrError)  //???
      eventualCmd <- cmdOrError match {
        case Right(cmd) =>
          cmd.pure[F]
        case Left(msg) =>
          tio.printError(msg) *>
              getCommand(tio, dummy)  // loop
      }
    } yield cmdOrError /*eventualCmd*/

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