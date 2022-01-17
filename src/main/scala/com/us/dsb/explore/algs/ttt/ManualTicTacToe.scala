package com.us.dsb.explore.algs.ttt


import cats.syntax.option._
import cats.syntax.either._
import enumeratum.{Enum, EnumEntry}

import scala.annotation.tailrec

object ManualTicTacToe extends App {


  sealed trait Command extends EnumEntry
  object Command /*extends Enum[Command]*/ {
    //val values: IndexedSeq[Command] = findValues

    case object Up extends Command
    case object Down extends Command
    case object Left extends Command
    case object Right extends Command
    case object Mark extends Command
    case object Quit extends Command
  }

  sealed trait Player extends EnumEntry
  object Player {
    case object O extends Player
    case object X extends Player
  }

  def parseCommand(rawCmd: String): Either[String, Command] = {
    import Command._
    rawCmd match {
      case "u" => Up.asRight
      case "d" => Down.asRight
      case "l" => Left.asRight
      case "r" => Right.asRight
      case "m" => Mark.asRight
      case "q" => Quit.asRight
      case _   =>
        s"Invalid input \"$rawCmd\"; try u(p), d(own), l(eft), r(right), m(ark), or q(uit)".asLeft
    }
  }

  @tailrec
  def getCommand(player: Player): Command = {
    // ?? clean looking more (was while mess., now recursive; is there better Scala way?

    // ?? clean embedded reference to stdin/console and stdout
    //print(s"Player $player command (u(p), d(own), l(eft), r(right), m(ark), q(uit): ")
    print(s"Player $player command?: ")
    val rawCmd = scala.io.StdIn.readLine()

    parseCommand(rawCmd) match {
      case Right(cmd) => cmd
      case Left(msg) =>
        println(msg)
        getCommand(player)
    }
  }

  // ?? somewhere expand to allow for history (maybe via Semigroup or whatever has .compose?)
  case class GameState(currentPlayer: Player)

  type Result = String
  def doCommandLoop(state: GameState): Result = {

    val command = getCommand(state.currentPlayer)

    import Command._

    command match {
      case Quit => "some result"
      case Up
         | Down
         | Left
         | Right =>
        println("TBD: move selection")
        doCommandLoop(state)
      case Mark =>
        println("TBD: make and switch player")
        import Player._
        val newPlayer = state.currentPlayer match {
          case X => O
          case O => X
        }
        doCommandLoop(state.copy(currentPlayer =  newPlayer))
    }
  }

  val initialState = GameState(Player.X)

  val gameResult = doCommandLoop(initialState)
  println("gameResult = " + gameResult)

}
