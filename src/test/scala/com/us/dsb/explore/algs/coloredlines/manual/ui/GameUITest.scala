package com.us.dsb.explore.algs.coloredlines.manual.ui

import com.us.dsb.explore.algs.coloredlines.manual.ui.GameUI.UICommand
import org.scalatest.AppendedClues._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

private[manual] class GameUITest extends AnyFunSpec {

  // Crude, manual stub and spy SegregatedTextIO.
  class SegregatedTextIODouble(inputLines: String*) extends SegregatedTextIO {
    private[this] var remainingInputs = inputLines
    private[this] var printedStringsReversed: List[String] = Nil
    // (no tracking of via which method wrote string)
    def getPrintedStrings: List[String] = printedStringsReversed.reverse

    override def printStateText(lineOrLines: String): Unit = {
      printedStringsReversed ::= lineOrLines
    }

    override def readPromptedLine(prompt: String): String = {
      printedStringsReversed ::= prompt
      remainingInputs match {
        case head +: tail =>
          remainingInputs = tail
          head
        case Nil =>
          ???
      }
    }

    override def printError(fullLine: String): Unit = {
      printedStringsReversed ::= fullLine
    }

    override def printResult(lineOrLines: String): Unit = {
      printedStringsReversed ::= lineOrLines
    }

  }


  describe("XxgetCommand:") {
    import org.scalatest.PrivateMethodTester._
    val getCommand = PrivateMethod[UICommand](Symbol("getCommand"))

    describe("Xxfor some valid command:") {
//      object LazyShared {
//        val ioDouble = new SegregatedTextIODouble("u")
//        val callResult = XxGameUI invokePrivate getCommand(ioDouble, XxxPlayer.X)
//      }
//      import LazyShared._
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
//        val ioDouble = new SegregatedTextIODouble("?", "u")
//        val callResult = XxGameUI invokePrivate getCommand(ioDouble, XxxPlayer.X)
//      }
//      import LazyShared._
//
//      it("Xxshould emit prompt, error, and second prompt line") {
//        ioDouble.getPrintedStrings shouldBe
//            List("XxxPlayer X command?: ",
//                 "Invalid input \"?\"; try u(p), d(own), l(eft), r(right), m(ark), or q(uit)",
//                 "XxxPlayer X command?: ")
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

  describe("XxrunGame, just end to end (commands to game result):") {
    def runViaStrings(inputs: String*): String =
      GameUI.runGame(new SegregatedTextIODouble(inputs: _*)).text
    def runViaChars(inputChars: String): String =
      runViaStrings(inputChars.map("" + _): _*)


    describe("Xx'q' should quit (result in message mentioning \"quit\"):") {
      it("Xx'q' as first command") {
        runViaStrings("q") should include regex ("(?i)quit")
      }
    }
    describe("""XxX win should report X won; text should:""") {
      lazy val actual = runViaChars("mdmrmdmrm")  // Xx: X--/OX-/-OX
      it("""Xxmention "win""") {
        actual should include regex "(?i)W[o]n"
      }
      it("""Xxmention X""") {
        actual should include ("X")
      }
      it("""Xxnot mention O (probably)""" ) { // not if "X beat O"
        actual should not include ("O")
      }
      it("""Xxbe "XxxPlayer X won" (currently) """) {
        actual should be ("XxxPlayer X won")
      }
    }
    describe("""XxO win should report O won; text should:""") {
      lazy val actual = runViaChars("m rm dlm rm rm dlm")  // Xx: XO-/XOX/--O
      it("""Xxmention win, O, and not X (probably)""") {
        actual should (
            include regex ("(?i)W[o]n")
            and include ("O")
            and not include ("X")
            )
      }
      it("""Xxbe "XxxPlayer O won" (currently) """) {
        actual should be ("XxxPlayer O won")
      }
    }
    describe("Xxdraw should report draw; text should:") {
      /*
         XXO/OOX/XXO:
        (1, 1) - m
        (2, 1) - dm
        (1, 2) - urm
        (2, 2) - dm
        (3, 1) - dlm
        (1, 3) - uurrm
        (2, 3) - dm
        (3, 3) - dm
        (3, 2) - lm
       */
      val cmds = "m" + "dm" + "urm" + "dm" + "dlm" + "uurrm" + "dm" + "dm" + "lm"
      lazy val actual = runViaChars(cmds)

      it("""Xxmention "draw""") {
        actual should include regex "(?i)Draw"
      }
      it("""Xxbe "Game ended in draw" (currently)""") {
        actual should be ("Game ended in draw")
      }
    }
  }
  ignore("try runGame, layer test with ~mocked GameState?") {
  }
  ignore("no coverage: markAtSelection's error case") {
  }
}
