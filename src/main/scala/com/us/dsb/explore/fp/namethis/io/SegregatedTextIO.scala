package com.us.dsb.explore.fp.namethis.io

import cats.effect.IO

private[io] trait SegregatedTextIO {
  def printStateText(lineOrLines: String): IO[Unit]
  def readPromptedLine(prompt: String): IO[String]
  def printError(fullLine: String): IO[Unit]
  def printResult(lineOrLines: String): IO[Unit]
}

private[io] class BaseConsoleTextIO(cio: ConsoleIO) extends SegregatedTextIO {
  override def printStateText(lineOrLines: String): IO[Unit] = cio.println(lineOrLines)
  override def readPromptedLine(prompt: String): IO[String]  = cio.readLine(prompt)
  override def printError(fullLine: String): IO[Unit] = cio.println(fullLine)
  override def printResult(lineOrLines: String): IO[Unit] = cio.println(lineOrLines)
}

private[io] class PlainConsoleTextIO(cio: ConsoleIO) extends BaseConsoleTextIO(cio)
object LivePlainConsoleTextIO extends PlainConsoleTextIO(LiveConsoleIO)
// (Expect to have test version in tests.)

private[io] class ColoredConsoleTextIO(cio: ConsoleIO) extends BaseConsoleTextIO(cio) {
  import scala.io.AnsiColor._
  override def readPromptedLine(prompt: String): IO[String] =
    super.readPromptedLine(BLUE + prompt + RESET)
  override def printError(fullLine: String): IO[Unit] =
    super.printError(RED + fullLine + RESET)
  override def printResult(lineOrLines: String): IO[Unit] =
    super.printResult(BOLD + lineOrLines + RESET)
}
private[io] object LiveColoredConsoleTextIO extends ColoredConsoleTextIO(LiveConsoleIO)
// (Expect to have test-double version in tests.)
