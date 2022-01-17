package com.us.dsb.explore.algs.ttt


import cats.syntax.option._
import cats.syntax.either._

import scala.annotation.tailrec

object ManualTicTacToe extends App {


  // ?? revisit (Enumeratum enum.?)
  object Command extends Enumeration {
    val Up, Down, Left, Right, Mark, Quit = Value
  }
  type Command = Command.Value

  object Player extends Enumeration {
    val X, O = Value
  }

  type Player = Player.Value

  def parseCommand(rawCmd: String): Either[String, Command] = {
    import Command._
    rawCmd match {
      case "u" => Up.asRight
      case "d" => Down.asRight
      case "l" => Left.asRight
      case "r" => Right.asRight
      case "m" => Mark.asRight
      case "q" => Quit.asRight
      case _ => s"Invalid command input line: \"$rawCmd\"".asLeft
    }
  }

  @tailrec
  def getCommand(player: Player): Command = {
    // ?? clean looking more (was while mess., now recursive; is there better Scala way?

    // ?? clean embedded reference to stdin/console and stdout
    print(s"$player, enter command (u(p), d(own), l(eft), r(rigjt), m(ark), q(uit): ")
    val rawCmd = scala.io.StdIn.readLine()

    parseCommand(rawCmd) match {
      case Right(cmd) => cmd
      case Left(msg) =>
        println(msg)
        getCommand(player)
    }
  }

  def doCommandLoop() = {

    val command = getCommand(Player.X)
    println("command = " + command)
    import Command._

    command match {
      case Quit => "some result"
      // doCommandLoop()
    }
  }
  doCommandLoop()



}
