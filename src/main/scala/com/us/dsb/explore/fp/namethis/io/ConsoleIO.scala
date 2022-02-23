package com.us.dsb.explore.fp.namethis.io

import cats.effect.IO

// Doing ConsoleIO as separate layer to have more simple layers for exploring
// testing, ZIO, etc.
trait ConsoleIO {
  def println(lineOrLines: String): IO[Unit]
  def readLine(prompt: String): IO[String]
}
object LiveConsoleIO extends ConsoleIO {

  override def println(lineOrLines: String): IO[Unit] =
    IO(Predef.println(lineOrLines))

  override def readLine(prompt: String): IO[String] =
    IO(scala.io.StdIn.readLine(prompt))
}
