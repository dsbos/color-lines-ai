//??? BEING REWORKED tagless-final(?) form:
package com.us.dsb.explore.fp.namethis.tf

import cats.Applicative
import cats.effect.IO/**/
import org.scalatest.LoneElement
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import cats.syntax.applicative._  // for .pure[...]


class ConsoleIOTest extends AnyFunSpec {

  it/*describe*/("TextIO? (disabled in original):") {
    cancel()
    it("NT.println ...") {
    }
    it("NT.readLine ...") {
    }
  }

  // Crude, manual stub and spy ConsoleIO.
  class ConsoleIODouble[F[_]: Applicative](inputLines: String* /* ???? F[_]? */) extends ConsoleIO[F] {
    private var stringsToRead = inputLines
    private var printedStringsReversed: List[String] = Nil;
    def getPrintedStrings: List[String] = printedStringsReversed.reverse  // ???? F[_]?

    override def println(lineOrLines: String): F[Unit] = {
      printedStringsReversed ::= lineOrLines
      ().pure[F]
    }

    override def readLine(prompt: String): F[String] = {
      printedStringsReversed ::= prompt

      stringsToRead match {
        case head +: tail =>
          stringsToRead = tail
          head.pure[F]
        case _ =>
          ???
      }
    }
  }

  describe("NT.ColoredConsoleTextIO") {
    import org.scalatest.LoneElement._

    def getUUT[F[_]](consoleIODouble: ConsoleIO[F]): SegregatedTextIO[F] = {  //???? still F or now IO/etc.?
      // Demo:  Try injecting "bad" UUT and see how failing conditions show up:
      new ColoredConsoleTextIO(consoleIODouble)
      //new PlainConsoleTextIO(consoleIODouble)
    }

    describe("NT.printStateText should print given text plainly; output should:") {
      object BreakpointableLazy {
        private val consoleIODouble = new ConsoleIODouble[IO]
        private val uut = getUUT(consoleIODouble)
        uut.printStateText("text")
        val printedStrings = consoleIODouble.getPrintedStrings
      }
      //???? where  unsafeRun...?
      import BreakpointableLazy._
      it("NT.include given text") {
        printedStrings.loneElement should include ("text")
      }
      it("NT.not include color/decoration escape sequences") {
        printedStrings.loneElement should not include (scala.io.AnsiColor.RED.take(1))
      }
      it("NT.include only given text") {
        printedStrings.loneElement should be ("text")
      }
    }

    // Demo:  Checking subconditions in multiple leaf-level tests:
    describe("NT.printError should print given text in red; output should:") {
      // Note:  "lazy" avoids executing during ScalaTest's registration phase.
      lazy val printedStrings = {
        val consoleIODouble = new ConsoleIODouble[IO]
        val uut = getUUT(consoleIODouble)

        uut.printError("given text")

        consoleIODouble.getPrintedStrings
      }
      it("NT.include given text") {
        printedStrings.loneElement should include ("given text")
      }
      it("NT.include red escape sequence") {
        printedStrings.loneElement should include (scala.io.AnsiColor.RED) // ?? in order too
      }
      it("NT.include reset escape sequence") {
        printedStrings.loneElement should include (scala.io.AnsiColor.RESET)  // ?? in order too
      }
      it("NT.not include only given text") {
        printedStrings.loneElement should not be ("given text")
      }
    }

    // Demo:  checking subconditions with "should ... and ..." (in one test):
    it("NT.printResult should print given text in bold (should ... and ...)") {
      val consoleIODouble = new ConsoleIODouble[IO]
      val uut = getUUT(consoleIODouble)

      uut.printResult("given text")
      val printedStrings = consoleIODouble.getPrintedStrings

      // Note:  "should ... and ..." reports only _first_condition that failed
      // (though error message does seem to include actual value).
      printedStrings.loneElement should (
          include ("given text") and
              include (scala.io.AnsiColor.BOLD) and
              include (scala.io.AnsiColor.RESET) and
              not be ("given text"))
    }

    describe("NT.readPromptedLine should print given prompt value and get input") {
      lazy val (printedStrings, lineRead) = {
        val consoleIODouble = new ConsoleIODouble[IO]("given input")
        val uut = getUUT(consoleIODouble)

        val lineRead = uut.readPromptedLine("given text")

        (consoleIODouble.getPrintedStrings, lineRead)
      }
      describe("NT.should print given prompted text in blue; output should:") {
        it("NT.include given text") {
          printedStrings.loneElement should include("given text")
        }
        it("NT.include blue escape sequence") {
          printedStrings.loneElement should include(scala.io.AnsiColor.BLUE) // ?? in order too
        }
        it("NT.include reset escape sequence") {
          printedStrings.loneElement should include(scala.io.AnsiColor.RESET) // ?? in order too
        }
        it("NT.not include only given text") {
          printedStrings.loneElement should not be ("given text")
        }
      }
      ignore("NT.should read input line text") {
        lineRead should be ("given input")
      }
    }

  }

}
