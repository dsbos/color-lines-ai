package com.us.dsb.explore.fp.namethis.ui

import org.scalatest.LoneElement
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ConsoleIOTest extends AnyFunSpec {

  it/*describe*/("TextIO?:") {
    cancel()
    it("println ...") {
    }
    it("readLine ...") {
    }
  }

  // Crude, manual stub and spy ConsoleIO.
  class ConsoleIODouble(inputLines: String*) extends ConsoleIO {
    private var stringsToRead = inputLines
    private var printedStrings: List[String] = Nil;
    def getPrintedStrings: List[String] = printedStrings

    override def println(lineOrLines: String): Unit = {
      printedStrings ::= lineOrLines
    }

    override def readLine(prompt: String): String = {
      printedStrings ::= prompt

      stringsToRead match {
        case head +: tail =>
          stringsToRead = tail
          head
        case _ =>
          ???
      }
    }
  }

  describe("ColoredConsoleTextIO") {
    import org.scalatest.LoneElement._

    def getUUT(consoleIODouble: ConsoleIO): SegregatedTextIO = {
      // Demo:  Try injecting "bad" UUT and see how failing conditions show up:
      new ColoredConsoleTextIO(consoleIODouble)
      //new PlainConsoleTextIO(consoleIODouble)
    }

    describe("printStateText should print given text plainly; output should:") {
      lazy val printedStrings = {
        val consoleIODouble = new ConsoleIODouble
        val uut = getUUT(consoleIODouble)

        uut.printStateText("text")

        consoleIODouble.getPrintedStrings
      }
      it("include given text") {
        printedStrings.loneElement should include ("text")
      }
      it("not include color/decoration escape sequences") {
        printedStrings.loneElement should not include (scala.io.AnsiColor.RED.take(1))
      }
      it("include only given text") {
        printedStrings.loneElement should be ("text")
      }
    }

    // Demo:  Checking subconditions in multiple leaf-level tests:
    describe("printError should print given text in red; output should:") {
      // Note:  "lazy" avoids executing during ScalaTest's registration phase.
      lazy val printedStrings = {
        val consoleIODouble = new ConsoleIODouble
        val uut = getUUT(consoleIODouble)

        uut.printError("given text")

        consoleIODouble.getPrintedStrings
      }
      it("include given text") {
        printedStrings.loneElement should include ("given text")
      }
      it("include red escape sequence") {
        printedStrings.loneElement should include (scala.io.AnsiColor.RED) // ?? in order too
      }
      it("include reset escape sequence") {
        printedStrings.loneElement should include (scala.io.AnsiColor.RESET)  // ?? in order too
      }
      it("not include only given text") {
        printedStrings.loneElement should not be ("given text")
      }
    }

    // Demo:  checking subconditions with "should ... and ..." (in one test):
    it("printResult should print given text in bold (should ... and ...)") {
      val consoleIODouble = new ConsoleIODouble
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

    describe("readPromptedLine should print given prompt value and get input") {
      lazy val (printedStrings, lineRead) = {
        val consoleIODouble = new ConsoleIODouble("given input")
        val uut = getUUT(consoleIODouble)

        val lineRead = uut.readPromptedLine("given text")

        (consoleIODouble.getPrintedStrings, lineRead)
      }
      describe("should print given prompted text in blue; output should:") {
        it("include given text") {
          printedStrings.loneElement should include("given text")
        }
        it("include blue escape sequence") {
          printedStrings.loneElement should include(scala.io.AnsiColor.BLUE) // ?? in order too
        }
        it("include reset escape sequence") {
          printedStrings.loneElement should include(scala.io.AnsiColor.RESET) // ?? in order too
        }
        it("not include only given text") {
          printedStrings.loneElement should not be ("given text")
        }
      }
      it("should read input line text") {
        lineRead should be ("given input")
      }
    }

  }

}
