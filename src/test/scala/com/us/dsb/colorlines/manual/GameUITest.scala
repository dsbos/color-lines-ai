package com.us.dsb.colorlines.manual

import com.us.dsb.colorlines.manual.GameUI.UICommand

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

import scala.annotation.unused

class GameUITest extends AnyFunSpec {

  // Crude, manual stub and spy SegregatedTextIO.
  class SegregatedConsoleIODouble(inputLines: String*) extends SegregatedConsoleIO {
    private var remainingInputs = inputLines
    private var printedStringsReversed: List[String] = Nil
    // (no tracking of via which method wrote string)
    def getPrintedStrings: List[String] = printedStringsReversed.reverse

    override def printStateText(lineOrLines: String): Unit = {
      printedStringsReversed ::= lineOrLines
    }

    override def readPromptedLine(prompt: String): Option[String] = {
      printedStringsReversed ::= prompt
      val result = remainingInputs.headOption
      if (remainingInputs.nonEmpty) {
        remainingInputs = remainingInputs.tail
      }
      result
    }

    override def printError(fullLine: String): Unit = {
      printedStringsReversed ::= fullLine
    }

    override def printResult(lineOrLines: String): Unit = {
      printedStringsReversed ::= lineOrLines
    }

  }


  describe("XxgetCommand:") {
    import org.scalatest.PrivateMethodTester.*
    @unused
    val getCommand = PrivateMethod[UICommand](Symbol("getCommand"))

    describe("Xxfor some valid command:") {
//      object LazyShared {
//        val ioDouble = SegregatedTextIODouble("u")
//        val callResult = XxGameUI invokePrivate getCommand(ioDouble, Player.X)
//      }
//      import LazyShared.*
//
//      it("Xxshould return decoded command") {
//        // Note: Can't break line line before withClue; "... . withClue(...)" and
//        //   '... \n .withClue(...)" don't attach clue; other options unclear.
//        //
//        callResult shouldBe XxUICommand.XxUp withClue
//            s"(printed strings: ${ioDouble.getPrintedStrings}"
//      }
//      it("Xxshould emit only prompt line (once)") {
//        ioDouble.getPrintedStrings should have length 1
//      }
    }

    describe("Xxfor invalid command(s) and then valid command:") {
//      object LazyShared {
//        val ioDouble = SegregatedTextIODouble("?", "u")
//        val callResult = XxGameUI invokePrivate getCommand(ioDouble, Player.X)
//      }
//      import LazyShared.*
//
//      it("Xxshould emit prompt, error, and second prompt line") {
//        ioDouble.getPrintedStrings shouldBe
//            List("Player X command?: ",
//                 "Invalid input \"?\"; try u(p), d(own), l(eft), r(right), m(ark), or q(uit)",
//                 "Player X command?: ")
//        // Theoretically, check specifics.
//      }
//      it("Xxshould return decoded eventual valid command") {
//        // Note: Can't break line line before withClue; "... . withClue(...)" and
//        //   '... \n .withClue(...)" don't attach clue; other options unclear.
//        //
//        callResult shouldBe XxUICommand.XxUp withClue
//            s"(printed strings: ${ioDouble.getPrintedStrings}"
//      }
    }
  }

  describe("runGame, just end to end (commands to game result):") {
    def runViaStrings(inputs: String*): String =
      GameUI.runGame(SegregatedConsoleIODouble(inputs*)).text
    @unused
    def runViaChars(inputChars: String): String =
      runViaStrings(inputChars.map("" + _)*)

    describe("'q' should quit (result in message mentioning \"quit\"):") {
      it("'q' as first command") {
        runViaStrings("q") should include regex "(?i)quit"
      }
    }

    describe("??? more TBD") {
      it("??? more TBD") {
      }
    }
  }
  ignore("try runGame, layer test with ~mocked GameState?") {
  }
  ignore("no coverage yet: markAtSelection's error case") {
  }
}
