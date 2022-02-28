//??? BEING REWORKED tagless-final(?) form:
package com.us.dsb.explore.fp.namethis.tf

import cats.{Applicative, Id}
import com.us.dsb.explore.fp.namethis.tf.TextIOClient.UICommand
//?????import cats.effect.IO
import cats.syntax.applicative._
import org.scalatest.AppendedClues._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class TextIOClientTest extends AnyFunSpec {

  // Crude, manual stub and spy SegregatedTextIO.
  class SegregatedTextIODouble[F[_]: Applicative](inputLines: String* /* ???? F[_]? */) extends SegregatedTextIO[F] {
    private var remainingInputs = inputLines
    private var printedStringsReversed: List[String] = Nil  // ???? State?
    // (no tracking of via which method wrote string)
    def getPrintedStrings: List[String] = printedStringsReversed.reverse  // ???? F[_]?

    override def printStateText(lineOrLines: String): F[Unit] = {
      printedStringsReversed ::= lineOrLines
      ().pure[F]
    }

    override def readPromptedLine(prompt: String): F[String] = {
      printedStringsReversed ::= prompt
      remainingInputs match {
        case head +: tail =>
          remainingInputs = tail
          head.pure[F]
        case Nil =>
          ???
      }
    }

    override def printError(fullLine: String): F[Unit] = {
      printedStringsReversed ::= fullLine
      ().pure[F]
    }

    override def printResult(lineOrLines: String): F[Unit] = {
      printedStringsReversed ::= lineOrLines
      ().pure[F]
    }

  }


  describe("NT.getCommand:") {
    import org.scalatest.PrivateMethodTester._
    val getCommand = PrivateMethod[UICommand](Symbol("getCommand"))

    describe("NT.for some valid command:") {
      object LazyShared {
        val ioDouble = new SegregatedTextIODouble[Id]("u")
        val call = TextIOClient.getCommand(ioDouble, "<dummy X>")
        val callResult = call/*.unsafeRunSync()*/
      }
      import LazyShared._

      it("NT.should return decoded command") {
        // Note: Can't break line line before withClue; "... . withClue(...)" and
        //   '... \n .withClue(...)" don't attach clue; other options unclear.
        //
        callResult shouldBe UICommand.Up withClue
            s"(printed strings: ${ioDouble.getPrintedStrings}"
      }
      it("NT.should emit prompt line and 'result = ' line ") {
        ioDouble.getPrintedStrings should have length 2 withClue
            s"(printed strings: ${ioDouble.getPrintedStrings}"
      }
    }

    describe("NT.for invalid command(s) and then valid command:") {
      object LazyShared {
        val ioDouble = new SegregatedTextIODouble[Id]("?", "u")
        val call = TextIOClient.getCommand(ioDouble, "<dummy X>")
        val callResult = call/*.unsafeRunSync()*/
      }
      import LazyShared._

      it("NT.should emit prompt, error, and second prompt line, plus `result =` lines ") {
        ioDouble.getPrintedStrings shouldBe
            List("Player <dummy X> command?: ",
                 "(Parsing result = Left(Invalid input \"?\"; try u(p), d(own), l(eft), r(right), m(ark), or q(uit)))",
                 "Invalid input \"?\"; try u(p), d(own), l(eft), r(right), m(ark), or q(uit)",
                 "Player <dummy X> command?: ",
                 "(Parsing result = Right(Up))")
      }
      it("NT.should return decoded eventual valid command") {
        // Note: Can't break line line before withClue; "... . withClue(...)" and
        //   '... \n .withClue(...)" don't attach clue; other options unclear.
        //
        callResult shouldBe UICommand.Up withClue
            s"(printed strings: ${ioDouble.getPrintedStrings}"
      }
    }

  }

}
