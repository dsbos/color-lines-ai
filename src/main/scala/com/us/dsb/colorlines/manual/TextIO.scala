package com.us.dsb.colorlines.manual

// ?? TODO:  Probably use separate files for GenericConsoleIO/etc. vs. SegregatedConsoleIO/etc.

private[manual] trait GenericConsoleIO {

  /** Writes given string plus line terminator to output. */
  def println(lineOrLines: String): Unit

  /** Reads input line after writing given prompt string.
   *  EOF returns `None` (instead of null) */
  def readLine(prompt: String): Option[String]
}

private object LiveGenericConsoleIO extends GenericConsoleIO {
  override def println(lineOrLines: String): Unit = Predef.println(lineOrLines)
  override def readLine(prompt: String): Option[String] = Option(scala.io.StdIn.readLine(prompt))
}

// (Expect to have test-double version in tests.)

// ?? TODO:  Maybe rename "segregated" to "specific" (vs. "generic"; shorter):

private[manual] trait SegregatedConsoleIO {
  private[manual] def printStateText(lineOrLines: String): Unit
  private[manual] def readPromptedLine(prompt: String): Option[String]
  private[manual] def printError(fullLine: String): Unit
  private[manual] def printResult(lineOrLines: String): Unit
}

private[manual] class BaseSegregatedConsoleIO(cio: GenericConsoleIO) extends SegregatedConsoleIO {
  private[manual] override def printStateText(lineOrLines: String): Unit = cio.println(lineOrLines)
  private[manual] override def readPromptedLine(prompt: String): Option[String]  = cio.readLine(prompt)
  private[manual] override def printError(fullLine: String): Unit = cio.println(fullLine)
  private[manual] override def printResult(lineOrLines: String): Unit = cio.println(lineOrLines)
}

private class PlainSegregatedConsoleIO(cio: GenericConsoleIO) extends BaseSegregatedConsoleIO(cio)

private object LivePlainSegregatedConsoleIO extends PlainSegregatedConsoleIO(LiveGenericConsoleIO)

// (Expect to have test-double version in tests.)

private[manual] class ColoredSegregatedConsoleIO(cio: GenericConsoleIO) extends BaseSegregatedConsoleIO(cio) {
  import scala.io.AnsiColor.*
  private[manual] override def readPromptedLine(prompt: String): Option[String] =
    super.readPromptedLine(BLUE + prompt + RESET)
  private[manual] override def printError(fullLine: String): Unit =
    super.printError(RED + fullLine + RESET)
  private[manual] override def printResult(lineOrLines: String): Unit =
    super.printResult(BOLD + lineOrLines + RESET)
}

private[manual] object LiveColoredSegregatedConsoleIO extends ColoredSegregatedConsoleIO(LiveGenericConsoleIO)

// (Expect to have test-double version in tests.)
