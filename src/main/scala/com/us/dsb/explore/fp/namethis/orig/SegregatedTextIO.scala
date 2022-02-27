package com.us.dsb.explore.fp.namethis.orig

private[orig] trait SegregatedTextIO {
  def printStateText(lineOrLines: String): Unit
  def readPromptedLine(prompt: String): String
  def printError(fullLine: String): Unit
  def printResult(lineOrLines: String): Unit
}

private[orig] class BaseConsoleTextIO(cio: ConsoleIO) extends SegregatedTextIO {
  override def printStateText(lineOrLines: String): Unit = cio.println(lineOrLines)
  override def readPromptedLine(prompt: String): String  = cio.readLine(prompt)
  override def printError(fullLine: String): Unit = cio.println(fullLine)
  override def printResult(lineOrLines: String): Unit = cio.println(lineOrLines)
}

private[orig] class PlainConsoleTextIO(cio: ConsoleIO) extends BaseConsoleTextIO(cio)
object LivePlainConsoleTextIO extends PlainConsoleTextIO(LiveConsoleIO)
// (Expect to have test version in tests.)

private[orig] class ColoredConsoleTextIO(cio: ConsoleIO) extends BaseConsoleTextIO(cio) {
  import scala.io.AnsiColor._
  override def readPromptedLine(prompt: String): String =
    super.readPromptedLine(BLUE + prompt + RESET)
  override def printError(fullLine: String): Unit =
    super.printError(RED + fullLine + RESET)
  override def printResult(lineOrLines: String): Unit =
    super.printResult(BOLD + lineOrLines + RESET)
}
private[orig] object LiveColoredConsoleTextIO extends ColoredConsoleTextIO(LiveConsoleIO)
// (Expect to have test-double version in tests.)
