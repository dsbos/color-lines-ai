package com.us.dsb.explore.fp.namethis.ui

trait SegregatedTextIO {
  def printStateText(lineOrLines: String): Unit
  def readPromptedLine(prompt: String): String
  def printError(fullLine: String): Unit
  def printResult(lineOrLines: String): Unit
}

class BaseConsoleTextIO(cio: ConsoleIO) extends SegregatedTextIO {
  override def printStateText(lineOrLines: String): Unit = cio.println(lineOrLines)
  override def readPromptedLine(prompt: String): String  = cio.readLine(prompt)
  override def printError(fullLine: String): Unit = cio.println(fullLine)
  override def printResult(lineOrLines: String): Unit = cio.println(lineOrLines)
}

class PlainConsoleTextIO(x: ConsoleIO) extends BaseConsoleTextIO(x)
object LivePlainConsoleTextIO extends PlainConsoleTextIO(LiveConsoleIO)
// (Expect to have test version in tests.)

class ColoredConsoleTextIO(x: ConsoleIO) extends BaseConsoleTextIO(x) {
  import scala.io.AnsiColor._
  override def readPromptedLine(prompt: String): String =
    super.readPromptedLine(BLUE + prompt + RESET)
  override def printError(fullLine: String): Unit =
    super.printError(RED + fullLine + RESET)
  override def printResult(lineOrLines: String): Unit =
    super.printResult(BOLD + lineOrLines + RESET)
}
object LiveColoredConsoleTextIO extends ColoredConsoleTextIO(LiveConsoleIO)
// (Expect to have test-double version in tests.)
