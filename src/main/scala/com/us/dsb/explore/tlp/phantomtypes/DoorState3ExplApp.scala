package com.us.dsb.explore.tlp.phantomtypes

//import scala.reflect.api.TypeTags
import scala.reflect.runtime.universe._


// NOT CLEANED

object DoorState3ExplApp extends App {

  trait OpennessState
  trait Open extends OpennessState
  trait Closed extends OpennessState

  trait AbstractDoor[state <: OpennessState] {
    def isOpen: String
  }
  object AbstractDoor {
    def apply[state <: OpennessState]()(implicit tt: TypeTag[state]) = {
      val x = if (tt.tpe =:= typeOf[Open]) "open1" else "closed1"
      new Door[state](isOpen = x)
    }

    def openDoor[state <: Closed](d: AbstractDoor[state]) = {
      new Door[Open](isOpen = "open2 (was " + d.isOpen + ")")
    }
    def closeDoor[state <: Open](d: AbstractDoor[state]) = {
      new Door[Closed](isOpen = "closed (was " + d.isOpen + ")")
    }
  }

  class Door[state <: OpennessState](val isOpen: String) extends AbstractDoor[state] {
    println("Door(isOpen = " + isOpen + ")")
  }


  //val d1d: AbstractDoor[Nothing] = AbstractDoor[Open]()
  val d1e: AbstractDoor[Open]    = AbstractDoor[Open]()
  //val d1f: AbstractDoor[Closed]  = AbstractDoor[Open]()
  //val d1g: AbstractDoor[Nothing] = AbstractDoor[Closed]()
  //val d1h: AbstractDoor[Open]    = AbstractDoor[Closed]()
  val d1i: AbstractDoor[Closed]  = AbstractDoor[Closed]()
  val d1a: AbstractDoor[Nothing] = AbstractDoor()
  val d1b: AbstractDoor[Open]    = AbstractDoor()
  val d1c: AbstractDoor[Closed]  = AbstractDoor()

  //val d2a: AbstractDoor[Open] = AbstractDoor.openDoor[Closed](d1e)
  //val d2b: AbstractDoor[Closed] = AbstractDoor.openDoor[Closed](d1e)
  //val d2c: AbstractDoor[Open] = AbstractDoor.closeDoor[Open](d1e)
  val d2d: AbstractDoor[Closed] = AbstractDoor.closeDoor[Open](d1e)

  val d2e: AbstractDoor[Open] = AbstractDoor.openDoor[Closed](d1i)
  //val d2f: AbstractDoor[Closed] = AbstractDoor.openDoor[Closed](d1i)
  //val d2g: AbstractDoor[Open] = AbstractDoor.closeDoor[Open](d1i)
  //val d2h: AbstractDoor[Closed] = AbstractDoor.closeDoor[Open](d1i)

  //val d2a2: AbstractDoor[Open] = AbstractDoor.openDoor(d1e)
  //val d2b2: AbstractDoor[Closed] = AbstractDoor.openDoor(d1e)
  //val d2c2: AbstractDoor[Open] = AbstractDoor.closeDoor(d1e)
  val d2d2: AbstractDoor[Closed] = AbstractDoor.closeDoor(d1e)

  val d2e2: AbstractDoor[Open] = AbstractDoor.openDoor(d1i)
  //val d2f2: AbstractDoor[Closed] = AbstractDoor.openDoor(d1i)
  //val d2g2: AbstractDoor[Open] = AbstractDoor.closeDoor(d1i)
  //val d2h2: AbstractDoor[Closed] = AbstractDoor.closeDoor(d1i)

  //????continue AbstractDoor.openDoor(d1e)

  //AbstractDoor.openDoor(d2e2)
  AbstractDoor.closeDoor(d2e2)
}
