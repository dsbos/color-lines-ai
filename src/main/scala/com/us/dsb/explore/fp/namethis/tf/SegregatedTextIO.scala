//??? TO BE reworked into tagless-final form:
package com.us.dsb.explore.fp.namethis.tf

//import cats.effect.IO

trait SegregatedTextIO[F[_]] {
  def printStateText(lineOrLines: String): F[Unit]
  def readPromptedLine(prompt: String): F[String]
  def printError(fullLine: String): F[Unit]
  def printResult(lineOrLines: String): F[Unit]
}

class BaseConsoleTextIO[F[_]](cio: ConsoleIO[F]) extends SegregatedTextIO[F] {
  override def printStateText(lineOrLines: String): F[Unit] = cio.println(lineOrLines)
  override def readPromptedLine(prompt: String): F[String]  = cio.readLine(prompt)
  override def printError(fullLine: String): F[Unit] = cio.println(fullLine)
  override def printResult(lineOrLines: String): F[Unit] = cio.println(lineOrLines)
}

class PlainConsoleTextIO[F[_]](cio: ConsoleIO[F]) extends BaseConsoleTextIO(cio)
object LivePlainConsoleTextIO extends PlainConsoleTextIO(LiveConsoleIO)
// (Expect to have test version in tests.)

class ColoredConsoleTextIO[F[_]](cio: ConsoleIO[F]) extends BaseConsoleTextIO(cio) {
  import scala.io.AnsiColor._
  override def readPromptedLine(prompt: String): F[String] =
    super.readPromptedLine(BLUE + prompt + RESET)
  override def printError(fullLine: String): F[Unit] =
    super.printError(RED + fullLine + RESET)
  override def printResult(lineOrLines: String): F[Unit] =
    super.printResult(BOLD + lineOrLines + RESET)
}
object LiveColoredConsoleTextIO extends ColoredConsoleTextIO(LiveConsoleIO)
// (Expect to have test-double version in tests.)
