package com.us.dsb.explore.algs.coloredlines.manual.ui

import org.scalatest.LoneElement
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

private[manual] class XxTextIOTest extends AnyFunSpec {

  it/*describe*/("TextIO?:") {
    cancel()
    it("XxXxprintln ...") {
    }
    it("XxXxreadLine ...") {
    }
  }

  // Crude, manual stub and spy ConsoleIO.
  class ConsoleIODouble(inputLines: String*) extends ConsoleIO {
    private var stringsToRead = inputLines
    private var printedStringsReversed: List[String] = Nil;
    def getPrintedStrings: List[String] = printedStringsReversed.reverse

    override def println(lineOrLines: String): Unit = {
      printedStringsReversed ::= lineOrLines
    }

    override def readLine(prompt: String): String = {
      printedStringsReversed ::= prompt

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
      it("XxXxinclude given text") {
        printedStrings.loneElement should include ("text")
      }
      it("XxXxnot include color/decoration escape sequences") {
        printedStrings.loneElement should not include (scala.io.AnsiColor.RED.take(1))
      }
      it("XxXxinclude only given text") {
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
      it("XxXxinclude given text") {
        printedStrings.loneElement should include ("given text")
      }
      it("XxXxinclude red escape sequence") {
        printedStrings.loneElement should include (scala.io.AnsiColor.RED) // ?? in order too
      }
      it("XxXxinclude reset escape sequence") {
        printedStrings.loneElement should include (scala.io.AnsiColor.RESET)  // ?? in order too
      }
      it("XxXxnot include only given text") {
        printedStrings.loneElement should not be ("given text")
      }
    }

    // Demo:  checking subconditions with "should ... and ..." (in one test):
    it("XxXxprintResult should print given text in bold (should ... and ...)") {
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
        it("XxXxinclude given text") {
          printedStrings.loneElement should include("given text")
        }
        it("XxXxinclude blue escape sequence") {
          printedStrings.loneElement should include(scala.io.AnsiColor.BLUE) // ?? in order too
        }
        it("XxXxinclude reset escape sequence") {
          printedStrings.loneElement should include(scala.io.AnsiColor.RESET) // ?? in order too
        }
        it("XxXxnot include only given text") {
          printedStrings.loneElement should not be ("given text")
        }
      }
      it("XxXxshould read input line text") {
        lineRead should be ("given input")
      }
    }

  }

}
