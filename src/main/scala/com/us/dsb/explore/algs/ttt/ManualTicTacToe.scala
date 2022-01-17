package com.us.dsb.explore.algs.ttt

import org.jline.terminal.TerminalBuilder

object ManualTicTacToe extends App {


  // ?? clean type-safety (w/Enumeratum enum.?)
  object Command extends Enumeration {
    val Up, Down, Left, Right, Mark, Quit = Value
  }
  type Command = Command.Value


  //type Command = String  // ?? clean type

  def getCommand(): Command = {
    // ?? clean looping (var, while, .isEmpty redundancy with None cases, explicit .get);
    //   possibly loop by calling sel recursively, but isn't there better Scala way?

    var command: Option[Command] = None
    while (command.isEmpty) {
      print("Enter command for <player>  (u(p), d(own), l(eft), r(rigjt), m(ark), q(uit): ")

      // ?? clean embedded reference to stdid/console
      val rawCmd = scala.io.StdIn.readLine()
      command = {
        import Command._
        rawCmd match {
          case "u" => Some(Up)
          case "d" => Some(Down)
          case "l" => Some(Left)
          case "r" => Some(Right)
          case "m" => Some(Mark)
          case "q" => Some(Quit)
          case _ => println(s"Invalid command input line: \"$rawCmd\""); None
        }
      }
    }
    command.get
  }
  val command = getCommand()
  println("command = " + command)

}
