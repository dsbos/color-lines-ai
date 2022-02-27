//??? BEING REWORKED tagless-final(?) form:
package com.us.dsb.explore.fp.namethis.tf

//import cats.effect.IO

private[fp] trait SegregatedTextIO[F[_]] {
  def printStateText(lineOrLines: String): F[Unit]
  def readPromptedLine(prompt: String): F[String]
  def printError(fullLine: String): F[Unit]
  def printResult(lineOrLines: String): F[Unit]
}

private[fp] class BaseConsoleTextIO[F[_]](cio: ConsoleIO[F]) extends SegregatedTextIO[F] {
  override def printStateText(lineOrLines: String): F[Unit] = cio.println(lineOrLines)
  override def readPromptedLine(prompt: String): F[String]  = cio.readLine(prompt)
  override def printError(fullLine: String): F[Unit] = cio.println(fullLine)
  override def printResult(lineOrLines: String): F[Unit] = cio.println(lineOrLines)
}

private[fp] class PlainConsoleTextIO[F[_]](cio: ConsoleIO[F]) extends BaseConsoleTextIO(cio)
private[fp] object LivePlainConsoleTextIO extends PlainConsoleTextIO(LiveConsoleIO)
// (Expect to have test version in tests.)

private[fp] class ColoredConsoleTextIO[F[_]](cio: ConsoleIO[F]) extends BaseConsoleTextIO(cio) {
  import scala.io.AnsiColor._
  override def readPromptedLine(prompt: String): F[String] =
    super.readPromptedLine(BLUE + prompt + RESET)
  override def printError(fullLine: String): F[Unit] =
    super.printError(RED + fullLine + RESET)
  override def printResult(lineOrLines: String): F[Unit] =
    super.printResult(BOLD + lineOrLines + RESET)
}
private[fp] object LiveColoredConsoleTextIO extends ColoredConsoleTextIO(LiveConsoleIO)
// (Expect to have test-double version in tests.)
