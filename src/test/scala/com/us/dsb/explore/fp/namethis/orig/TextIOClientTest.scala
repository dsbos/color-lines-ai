package com.us.dsb.explore.fp.namethis.orig

import com.us.dsb.explore.fp.namethis.orig.TextIOClient.UICommand
import org.scalatest.AppendedClues._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class TextIOClientTest extends AnyFunSpec {

  // Crude, manual stub and spy SegregatedTextIO.
  class SegregatedTextIODouble(inputLines: String*) extends SegregatedTextIO {
    private var remainingInputs = inputLines
    private var printedStrings: List[String] = Nil
    // (no tracking of via which method)
    def getPrintedStrings: List[String] = printedStrings

    override def printStateText(lineOrLines: String): Unit = {
      printedStrings ::= lineOrLines
    }

    override def readPromptedLine(prompt: String): String = {
      printedStrings ::= prompt
      remainingInputs match {
        case head +: tail =>
          remainingInputs = tail
          head
        case Nil =>
          ???
      }
    }

    override def printError(fullLine: String): Unit = {
      printedStrings ::= fullLine
    }

    override def printResult(lineOrLines: String): Unit = {
      printedStrings ::= lineOrLines
    }

  }


  describe("getCommand:") {
    import org.scalatest.PrivateMethodTester._
    val getCommand = PrivateMethod[UICommand](Symbol("getCommand"))

    describe("for some valid command:") {
      object LazyShared {
        val ioDouble = new SegregatedTextIODouble("u")
        val callResult = TextIOClient invokePrivate getCommand(ioDouble, "<dummuy X>")
      }
      import LazyShared._

      it("should return decoded command") {
        // Note: Can't break line line before withClue; "... . withClue(...)" and
        //   '... \n .withClue(...)" don't attach clue; other options unclear.
        //
        callResult shouldBe UICommand.Up withClue
            s"(printed strings: ${ioDouble.getPrintedStrings}"
      }
      it("should emit only prompt line (once)") {
        ioDouble.getPrintedStrings should have length 1
      }
    }

    describe("for invalid command(s) and then valid command:") {
      object LazyShared {
        val ioDouble = new SegregatedTextIODouble("?", "u")
        val callResult = TextIOClient invokePrivate getCommand(ioDouble, "<dummuy X>")
      }
      import LazyShared._

      it("should emit error line and extra prompt line") {
        ioDouble.getPrintedStrings should have length 3
        // Theoretically, check specifics.
      }
      it("should return decoded eventual valid command") {
        // Note: Can't break line line before withClue; "... . withClue(...)" and
        //   '... \n .withClue(...)" don't attach clue; other options unclear.
        //
        callResult shouldBe UICommand.Up withClue
            s"(printed strings: ${ioDouble.getPrintedStrings}"
      }
    }

  }

}
