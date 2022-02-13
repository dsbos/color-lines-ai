package com.us.dsb.explore.algs.ttt.manual.ui

import com.us.dsb.explore.algs.ttt.manual.ui.GameUI.GameUIResult

// Doing ConsoleIO as separate layer to have more simple layers for exploring
// testing, ZIO, etc.
private[ui] trait ConsoleIO {
  def println(lineOrLines: String): Unit
  def readLine(prompt: String): String
}
private[manual] object LiveConsoleIO extends ConsoleIO {
  override def println(lineOrLines: String): Unit = Predef.println(lineOrLines)
  override def readLine(prompt: String): String = scala.io.StdIn.readLine(prompt)
}
// (Expect to have test-double version in tests.)

// ?? revisit names:

private[ui] trait SegregatedTextIO {
  private[ui] def printStateText(lineOrLines: String): Unit
  private[ui] def readPromptedLine(prompt: String): String
  private[ui] def printError(fullLine: String): Unit
  // ?? first, second, or both?:
  private[ui] def printResult(lineOrLines: String): Unit
  private[ui] def printResult(result: GameUIResult): Unit = printResult(result.text)
}

private[ui] class BaseConsoleTextIO(cio: ConsoleIO) extends SegregatedTextIO {
  private[ui] override def printStateText(lineOrLines: String): Unit = cio.println(lineOrLines)
  private[ui] override def readPromptedLine(prompt: String): String  = cio.readLine(prompt)
  private[ui] override def printError(fullLine: String): Unit = cio.println(fullLine)
  private[ui] override def printResult(lineOrLines: String): Unit = cio.println(lineOrLines)
}

private[manual] class PlainConsoleTextIO(cio: ConsoleIO) extends BaseConsoleTextIO(cio)
object LivePlainConsoleTextIO extends PlainConsoleTextIO(LiveConsoleIO)
// (Expect to have test version in tests.)

class ColoredConsoleTextIO(cio: ConsoleIO) extends BaseConsoleTextIO(cio) {
  import scala.io.AnsiColor._
  private[ui] override def readPromptedLine(prompt: String): String =
    super.readPromptedLine(BLUE + prompt + RESET)
  private[ui] override def printError(fullLine: String): Unit =
    super.printError(RED + fullLine + RESET)
  private[ui] override def printResult(lineOrLines: String): Unit =
    super.printResult(BOLD + lineOrLines + RESET)
}
private[manual] object LiveColoredConsoleTextIO extends ColoredConsoleTextIO(LiveConsoleIO)
// (Expect to have test-double version in tests.)
