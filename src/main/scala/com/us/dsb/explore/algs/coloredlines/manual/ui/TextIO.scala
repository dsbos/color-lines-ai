package com.us.dsb.explore.algs.coloredlines.manual.ui

import com.us.dsb.explore.algs.coloredlines.manual.ui.GameUI.GameUIResult

// ?? TODO:  Probably use separate files for GenericConsoleIO/etc. vs. SegregatedConsoleIO/etc.

private[ui] trait GenericConsoleIO {

  /** Writes given string plus line terminator to output. */
  def println(lineOrLines: String): Unit

  /** Reads input line after writing given prompt string.
   *  EOF returns `None` (instead of null) */
  def readLine(prompt: String): Option[String]
}

private[this] object LiveGenericConsoleIO extends GenericConsoleIO {
  override def println(lineOrLines: String): Unit = Predef.println(lineOrLines)
  override def readLine(prompt: String): Option[String] = Option(scala.io.StdIn.readLine(prompt))
}

// (Expect to have test-double version in tests.)

// ?? TODO:  Maybe rename "segregated" to "specific" (vs. "generic"; shorter):

private[ui] trait SegregatedConsoleIO {
  private[ui] def printStateText(lineOrLines: String): Unit
  private[ui] def readPromptedLine(prompt: String): Option[String]
  private[ui] def printError(fullLine: String): Unit
  private[ui] def printResult(lineOrLines: String): Unit
}

private[ui] class BaseSegregatedConsoleIO(cio: GenericConsoleIO) extends SegregatedConsoleIO {
  private[ui] override def printStateText(lineOrLines: String): Unit = cio.println(lineOrLines)
  private[ui] override def readPromptedLine(prompt: String): Option[String]  = cio.readLine(prompt)
  private[ui] override def printError(fullLine: String): Unit = cio.println(fullLine)
  private[ui] override def printResult(lineOrLines: String): Unit = cio.println(lineOrLines)
}

private[this] class PlainSegregatedConsoleIO(cio: GenericConsoleIO) extends BaseSegregatedConsoleIO(cio)

private[this] object LivePlainSegregatedConsoleIO extends PlainSegregatedConsoleIO(LiveGenericConsoleIO)

// (Expect to have test-double version in tests.)

private[manual] class ColoredSegregatedConsoleIO(cio: GenericConsoleIO) extends BaseSegregatedConsoleIO(cio) {
  import scala.io.AnsiColor._
  private[ui] override def readPromptedLine(prompt: String): Option[String] =
    super.readPromptedLine(BLUE + prompt + RESET)
  private[ui] override def printError(fullLine: String): Unit =
    super.printError(RED + fullLine + RESET)
  private[ui] override def printResult(lineOrLines: String): Unit =
    super.printResult(BOLD + lineOrLines + RESET)
}

private[manual] object LiveColoredSegregatedConsoleIO extends ColoredSegregatedConsoleIO(LiveGenericConsoleIO)

// (Expect to have test-double version in tests.)
