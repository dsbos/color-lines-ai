package com.us.dsb.explore.algs.ttt.manual

import com.us.dsb.explore.algs.ttt.manual.GameUI.{GameUIResult, SegregatedTextIO}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._


class GameUITest extends AnyFunSpec {

  class CrudeManualStubIO(inputs: String*) extends SegregatedTextIO {

    private var remainingInputs = inputs.toList

    override def readPromptedLine(prompt: String): String = {
      Predef.println(s"readLine(prompt): not checking prompt '''${prompt}'''")
      remainingInputs match {
        case head :: tail =>
          remainingInputs = tail
          val thisInput = head
          Predef.println(s"readLine(prompt): returning '''${thisInput}'''")
          thisInput
        case Nil =>
          ???
      }
    }

    override def printError(fullLine: String): Unit = {
      Predef.println(s"printError(String):   not checking: '''${fullLine}'''")
    }
    override def printStateText(lineOrLines: String): Unit = {
      Predef.println(s"printStateText(String):   not checking: '''${lineOrLines}'''")
    }

  }

  private def runStrings(inputs: String*): String =
    GameUI.runGame(new CrudeManualStubIO(inputs: _*)).text
  private def runChars(inputChars: String): String =
    runStrings(inputChars.map("" + _): _*)


  describe("runGame, just end to end (commands to game result):") {
    describe("'q' should quit (result in message mentioning \"quit\"):") {
      it("'q' as first command") {
        runStrings("q") should include regex ("(?i)quit")
      }
    }
    describe("""X win should report X won; text should:""") {
      lazy val actual = runChars("mdmrmdmrm")  // X--/OX-/-OX
      it("""mention "win""") {
        actual should include regex "(?i)W[o]n"
      }
      it("""mention X""") {
        actual should include ("X")
      }
      it("""not mention O (probably)""" ) { // not if "X beat O"
        actual should not include ("O")
      }
      it("""be "Player X won" (currently) """) {
        actual should be ("Player X won")
      }
    }
    describe("""O win should report O won; text should:""") {
      lazy val actual = runChars("m rm dlm rm rm dlm")  // XO-/XOX/--O
      it("""mention win, O, and not X (probably)""") {
        actual should (
            include regex ("(?i)W[o]n")
            and include ("O")
            and not include ("X")
            )
      }
      it("""be "Player O won" (currently) """) {
        actual should be ("Player O won")
      }
    }
    describe("draw should report draw; text should:") {
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
      lazy val actual = runChars(cmds)

      it("""mention "draw""") {
        actual should include regex "(?i)Draw"
      }
      it("""be "Game ended in draw" (currently)""") {
        actual should be ("Game ended in draw")
      }
    }
  }
}
