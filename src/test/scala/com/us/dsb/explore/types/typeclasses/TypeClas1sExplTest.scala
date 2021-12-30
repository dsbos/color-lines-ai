package com.us.dsb.explore.types.typeclasses

//import scala.annotation.implicitNotFound

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers._


/**
  * Basic demo of type class.
  */
class TypeClas1sExplTest extends AnyFunSuite {

  // Type class declaration (always trait?):
  trait Dumpable[T] {
    // Some operation specific to type class:
    def dump1(v: T): String
  }

  // Client of type class and operation(?):
  def dump2[T](t: T)(implicit d: Dumpable[T]) = d.dump1(t)


  // Class to be logical member(?) of type class:
  case class ThingOne(d: Double)


  // Type class instance of Dumpable for ThingThing:
  implicit object DumpableThingOne extends Dumpable[ThingOne] {
    def dump1(t: ThingOne): String = {
      t.toString
    }
  }


  val x = ThingOne(1)


  println("x . dump2: " + dump2(x))

  test("") {
  }

}
