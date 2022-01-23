package com.us.dsb.explore.algs.ttt.manual.ui

import com.us.dsb.explore.algs.ttt.manual.ui.GameUI.GameUIResult

// ?? revisit namea

private[ui] trait SegregatedTextIO {
  private[ui] def printStateText(lineOrLines: String): Unit
  private[ui] def readPromptedLine(prompt: String): String
  private[ui] def printError(fullLine: String): Unit
  // ?? first, second, or both?:
  private[ui] def printResult(lineOrLines: String): Unit
  private[ui] def printResult(result: GameUIResult): Unit = printResult(result.text)
}

private[ui] class BaseConsoleTextIO extends SegregatedTextIO {
  import scala.io.StdIn.readLine

  private[ui] override def printStateText(lineOrLines: String): Unit = println(lineOrLines)
  private[ui] override def readPromptedLine(prompt: String): String  = readLine(prompt)
  private[ui] override def printError(fullLine: String): Unit = println(fullLine)
  private[ui] override def printResult(lineOrLines: String): Unit = println(lineOrLines)
}

object PlainConsoleTextIO extends BaseConsoleTextIO

object ColoredConsoleTextIO extends BaseConsoleTextIO {
  import scala.io.AnsiColor._
  private[ui] override def readPromptedLine(prompt: String): String =
    super.readPromptedLine(BLUE + prompt + RESET)
  private[ui] override def printError(fullLine: String): Unit =
    super.printError(RED + fullLine + RESET)
  private[ui] override def printResult(lineOrLines: String): Unit =
    super.printResult(BOLD + lineOrLines + RESET)
}
