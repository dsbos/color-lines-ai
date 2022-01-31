package com.us.dsb.explore.fp.namethis.tf


// Doing ConsoleIO as separate layer to have more simple layers for exploring
// testing, ZIO, etc.
trait ConsoleIO {
  def println(lineOrLines: String): Unit
  def readLine(prompt: String): String
}
object LiveConsoleIO extends ConsoleIO {
  override def println(lineOrLines: String): Unit = Predef.println(lineOrLines)
  override def readLine(prompt: String): String = scala.io.StdIn.readLine(prompt)
}
// (Expect to have test-double version in tests.)
