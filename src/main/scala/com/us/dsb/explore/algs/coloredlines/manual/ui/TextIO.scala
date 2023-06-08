package com.us.dsb.explore.algs.coloredlines.manual.ui

import com.us.dsb.explore.algs.coloredlines.manual.ui.GameUI.GameUIResult

// Doing GenericConsoleIO as separate layer to have more simple layers for exploring
// testing, ZIO, etc.
private[ui] trait GenericConsoleIO {

  /** Writes given string plus line terminator to output. */
  def println(lineOrLines: String): Unit

  /** Reads input line after writing given prompt string.
   * EOF returns None (instead of null) */
  def readLine(prompt: String): Option[String]
}

private[this] object LiveGenericConsoleIO extends GenericConsoleIO {
  override def println(lineOrLines: String): Unit = Predef.println(lineOrLines)
  override def readLine(prompt: String): Option[String] = Option(scala.io.StdIn.readLine(prompt))
}
// (Expect to have test-double version in tests.)

// ?? revisit names:

private[ui] trait SegregatedTextIO {
  private[ui] def printStateText(lineOrLines: String): Unit
  private[ui] def readPromptedLine(prompt: String): Option[String]
  private[ui] def printError(fullLine: String): Unit
  // ?? first, second, or both?:
  private[ui] def printResult(lineOrLines: String): Unit
  private[ui] def printResult(result: GameUIResult): Unit = printResult(result.text)
}

private[ui] class BaseConsoleTextIO(cio: GenericConsoleIO) extends SegregatedTextIO {
  private[ui] override def printStateText(lineOrLines: String): Unit = cio.println(lineOrLines)
  private[ui] override def readPromptedLine(prompt: String): Option[String]  = cio.readLine(prompt)
  private[ui] override def printError(fullLine: String): Unit = cio.println(fullLine)
  private[ui] override def printResult(lineOrLines: String): Unit = cio.println(lineOrLines)
}

private[this] class PlainConsoleTextIO(cio: GenericConsoleIO) extends BaseConsoleTextIO(cio)
private[this] object LivePlainConsoleTextIO extends PlainConsoleTextIO(LiveGenericConsoleIO)
// (Expect to have test version in tests.)

private[manual] class ColoredConsoleTextIO(cio: GenericConsoleIO) extends BaseConsoleTextIO(cio) {
  import scala.io.AnsiColor._
  private[ui] override def readPromptedLine(prompt: String): Option[String] =
    super.readPromptedLine(BLUE + prompt + RESET)
  private[ui] override def printError(fullLine: String): Unit =
    super.printError(RED + fullLine + RESET)
  private[ui] override def printResult(lineOrLines: String): Unit =
    super.printResult(BOLD + lineOrLines + RESET)
}
private[manual] object LiveColoredConsoleTextIO extends ColoredConsoleTextIO(LiveGenericConsoleIO)
// (Expect to have test-double version in tests.)
