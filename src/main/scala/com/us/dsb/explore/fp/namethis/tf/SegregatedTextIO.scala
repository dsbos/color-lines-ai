package com.us.dsb.explore.fp.namethis.tf

import cats.effect.IO

trait SegregatedTextIO {
  def printStateText(lineOrLines: String): IO[Unit]
  def readPromptedLine(prompt: String): IO[String]
  def printError(fullLine: String): IO[Unit]
  def printResult(lineOrLines: String): IO[Unit]
}

class BaseConsoleTextIO(cio: ConsoleIO) extends SegregatedTextIO {
  override def printStateText(lineOrLines: String): IO[Unit] = cio.println(lineOrLines)
  override def readPromptedLine(prompt: String): IO[String]  = cio.readLine(prompt)
  override def printError(fullLine: String): IO[Unit] = cio.println(fullLine)
  override def printResult(lineOrLines: String): IO[Unit] = cio.println(lineOrLines)
}

class PlainConsoleTextIO(x: ConsoleIO) extends BaseConsoleTextIO(x)
object LivePlainConsoleTextIO extends PlainConsoleTextIO(LiveConsoleIO)
// (Expect to have test version in tests.)

class ColoredConsoleTextIO(x: ConsoleIO) extends BaseConsoleTextIO(x) {
  import scala.io.AnsiColor._
  override def readPromptedLine(prompt: String): IO[String] =
    super.readPromptedLine(BLUE + prompt + RESET)
  override def printError(fullLine: String): IO[Unit] =
    super.printError(RED + fullLine + RESET)
  override def printResult(lineOrLines: String): IO[Unit] =
    super.printResult(BOLD + lineOrLines + RESET)
}
object LiveColoredConsoleTextIO extends ColoredConsoleTextIO(LiveConsoleIO)
// (Expect to have test-double version in tests.)
