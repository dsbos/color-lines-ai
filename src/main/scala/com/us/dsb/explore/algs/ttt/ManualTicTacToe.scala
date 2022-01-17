package com.us.dsb.explore.algs.ttt


import cats.syntax.option._
import cats.syntax.either._
import enumeratum.{Enum, EnumEntry}

import scala.annotation.tailrec

object ManualTicTacToe extends App {


  sealed trait Command extends EnumEntry
  sealed trait MoveCommand extends Command  // ?? whyy doesn't Command's "sealed" obvious this one?
  object Command {
    case object Up extends MoveCommand
    case object Down extends MoveCommand
    case object Left extends MoveCommand
    case object Right extends MoveCommand
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

  // ?? clean (probably refined Int, maybe enum.
  // 1: top row/leftmost column
  type Index = Int
  object Index {
  }

  // ?? somewhere expand to allow for history (maybe via Semigroup or whatever has .compose?)
  case class GameUIState(currentPlayer: Player,
                         selectedRow: Index,
                         selectedColumn: Index) {
    def selectionString: String = {
      s"<row $selectedRow / column $selectedColumn>"
    }

    private def wrapToRange(rawIncremented: Index): Index = {
      scala.math.floorMod((rawIncremented - 1), 3) + 1
    }

    def withRowAdustedBy(delta: Int): GameUIState = {
      copy(selectedRow = wrapToRange(selectedRow + delta))
    }
    def withColumnAdustedBy(delta: Int): GameUIState = {
      copy(selectedColumn = wrapToRange(selectedColumn + delta))
    }

  }

  case class GameResult(tbd: String)

  object UICommandMethods {
    def moveSelection(state: GameUIState, moveCommand: MoveCommand): GameUIState = {
      import Command._
      moveCommand match {
        case Up    => state.withRowAdustedBy(-1)
        case Down  => state.withRowAdustedBy(1)
        case Left  => state.withColumnAdustedBy(-1)
        case Right => state.withColumnAdustedBy(1)
      }
    }

    def markAtSelection(state: GameUIState): GameUIState = {
      import Player._

      // ?? soon: check valid (first, just count moves)
      println(s"TBD: mark board at ${state.selectionString}/make player move")

      val newPlayer = state.currentPlayer match {
        case X => O
        case O => X
      }
      state.copy(currentPlayer = newPlayer)
    }

    def doQuit(state: GameUIState): GameResult = {
      GameResult("some result")
    }
  }

  @tailrec
  def getAndDoUiCommands(state: GameUIState): GameResult = {
    println("state = " + state)
    val command = getCommand(state.currentPlayer)

    import Command._
    import UICommandMethods._
    command match {
      case move: MoveCommand => getAndDoUiCommands(moveSelection(state, move))
      case Mark              => getAndDoUiCommands(markAtSelection(state))
      case Quit              => doQuit(state)
    }
  }

  val initialState = GameUIState(Player.X, 1, 1)

  val gameResult = getAndDoUiCommands(initialState)
  println("gameResult = " + gameResult)

}
