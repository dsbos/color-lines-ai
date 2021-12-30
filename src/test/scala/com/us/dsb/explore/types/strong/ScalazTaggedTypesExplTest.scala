package com.us.dsb.explore.types.strong

import org.scalatest.funsuite.AnyFunSuite
//import org.scalatest.matchers._
import org.scalatest.matchers.should.Matchers._  // for "shouldNot compile"

import scalaz.Tag
import scalaz.Tags
import scalaz.@@   // Note: Definition depends on ScalaZ version.


class ScalazTaggedTypesExplTest extends AnyFunSuite {

  test("Tagged ...") {

    trait ThingA
    trait ThingB

    type ThingAInt = Int @@ ThingA
    type ThingBInt = Int @@ ThingB


    // assim: Tag(...) and other shorter versions of ".asInstanceOf[...]"

    /** Like regular case class companion object and apply(...). */
    object ThingAInt {
      def apply(i: Int): ThingAInt = i.asInstanceOf[ThingAInt]
    }
    implicit class IntToThingAInt(value: Int) /* extends AnyVal (but not nestable here) */ {
      def thingA: ThingAInt = value.asInstanceOf[ThingAInt]
    }

    val x1: ThingAInt = 1.asInstanceOf[ThingAInt]

    val x15: ThingAInt = ThingAInt(15)
    val x16: ThingAInt = 1.thingA

    val x2: ThingAInt = Tag(2)

    val x3 = Tag(3)             // bad: gets @@[Int, Nothing]

    val x4a = Tag[Int, ThingA](3)
    val x4b: ThingAInt = x4a

    val t1 = Tag.of[ThingA](1)
    val t2 = Tag.of[ThingAInt](x1)
    val t3 = Tag.unwrap(x1)

    Tag.unwrap(x1) + 1


    ////////////////////
    // Strong typing at simple assignment:
    val a1: ThingAInt = 1.asInstanceOf[ThingAInt]
    val a1b: ThingAInt = a1
    "val b1: ThingBInt = a1" shouldNot compile
    "val a2: ThingAInt = 2" shouldNot compile

    ////////////////////
    // Seemingly good strong typing in unconstrained expression ...
    val b2 = 2.asInstanceOf[ThingBInt]
    "a1 - b2" shouldNot compile

    ////////////////////
    // ... but loses access to "base" type's methods (broken relative to what
    // I wanted--unmixable versions of types still having normal operations as
    // in VHDL and Ada integer types):
    "val a0: ThingAInt = a1 - a1" shouldNot compile // well, _doesn't_, but should
    "a1 - a1" shouldNot compile // well, _doesn't_, but should
    // yield "value - is not a member of ThingAInt"

    // ???? add getting value out

    ////////////////////
    // Seemingly good strong typing in <what kind?> expression:
    "if (a1 > b2) {}" shouldNot compile

    ////////////////////
    // Don't forget some implicit AnyRef(?) -> String conversion:
    "a1 + a1" shouldNot compile  // (does not, but for interference reason)
  }

}
