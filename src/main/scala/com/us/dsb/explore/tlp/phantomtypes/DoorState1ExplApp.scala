package com.us.dsb.explore.tlp.phantomtypes

/**
  * Open/closed door state with phantom types only (no open/closed value),
  * and _static_ open/closed methods.
  * (From http://gigiigig.github.io/tlp-step-by-step/phantom-types.html/)
  */
object DoorState1ExplApp extends App {

  trait OpennessState
  trait Open extends OpennessState
  trait Closed extends OpennessState

  trait Door[state <: OpennessState]
  object Door {
    /** Can create in either Open or Closed state. */
    def apply[state <: OpennessState]() = new Door[state]() { /* (non-abstract) */ }

    // (State-changing methods are static (not on Door trait/class). */
    def openDoor[state <: Closed](d: Door[state]) = Door[Open]()
    def closeDoor[state <: Open](d: Door[state]) = Door[Closed]()
  }

  // Allows creating in either Open or Closed state:
  val d1a_o: Door[Open]   = Door[Open]()
  val d1b_c: Door[Closed] = Door[Closed]()

  // Allows inferring target type:
  val d1c_o /* : Door[Open]   with Object */ = Door[Open]()
  val d1d_c /* : Door[Closed] with Object */ = Door[Closed]()

  // Blocks assigning to (explicitly) wrong type:
  // val d1e_c: Door[Closed] = Door[Open]
  // val d1f_x: Door[Nothing] = Door[Open]
  // val d1g_x: Door[Int] = Door[Open]

  // Does NOT block not specifying a state:
  val d1g_x = Door()

  // Does NOT block some invalid states:
  val d1h_x: Door[OpennessState] = null
  val d1i_x: Door[Nothing] = null

  // Blocks some invalid states:
  // val d1j_x: Door[Int] = Door[Int]
  // val d1k_x: Door[Int] = null

  // ?? TBD: Whether it confirms to context's type
  val d1h_o: Door[Open]    = Door()
  val d1i_c: Door[Closed]  = Door()


  // Allows/blocks operations:
  val d2a_c: Door[Closed] = Door.closeDoor(d1a_o)
  //val d2b_c: Door[Open] = Door.openDoor(d1a_o)
  //val d2c_c = Door.openDoor(d1a_o)

  //val d2d_c: Door[Closed] = Door.closeDoor(d1b_c)
  val d2e_o: Door[Open] = Door.openDoor(d1b_c)
  //val d2f_c = Door.closeDoor(d1b_c)

  // Allows inferring target type:
  val d2e_c /* : Door[Closed] with Object */ = Door.closeDoor(d1a_o)
  val d2h_o /* : Door[Open]   with Object */ = Door.openDoor(d1b_c)
}
