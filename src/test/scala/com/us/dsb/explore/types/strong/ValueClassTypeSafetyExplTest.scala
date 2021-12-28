//????PURGE THIS / MOVE THIS OUT

package com.us.dsb.explore.types.strong

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers._
import org.scalatest.matchers.should.Matchers._  // for "shouldNot compile"

object ValueClassTypeSafetyExplTest {

  case class PoiId(value: Int) extends AnyVal

  case class SpNumber(value: Int) extends AnyVal

  case class MovingTime(value: Int) extends AnyVal {
    // println() - does not compile:
    // "Error:(16, 12) this statement is not allowed in value class"
  }

}


/**
  * ... shows that value classes are good for strong typing with simple parameter
  * passing and assignment, but not good for strong typing with operations in
  * expressions ...
  */
class ValueClassTypeSafetyExplTest extends AnyFunSuite {
  // (Review:  I thought object's members were automatically visible in companion
  // class, but they weren't in this case.)
  import ValueClassTypeSafetyExplTest._


  case class Bundle(origPoi: PoiId, origSp: SpNumber)
  val somePoi = PoiId(111)
  val someSpNum = SpNumber(0)
  var anotherPoi: PoiId = PoiId(0)


  test("Strong typing for simple parameter passing and assignment.") {

    // Allows correct type (same value class):
    Bundle(somePoi, someSpNum)
    anotherPoi = somePoi

    // Catches wrong type--different value class of same underlying type):
    "Bundle(someSpNum, somePoi)" shouldNot typeCheck
    "anotherPoi = someSpNum" shouldNot typeCheck
    // Error:(...) type mismatch;
    //    found   : com.savi.spark.app.summarizer.TypeExplorationTest.SpNumber
    //    required: com.savi.spark.app.summarizer.TypeExplorationTest.PoiId

    // Catches wrong type--type underlying value class:
    "Bundle(42, someSp, 42)" shouldNot typeCheck
    "anotherPoi = 42" shouldNot typeCheck
    // Error:(...) type mismatch;
    //    found   : Int(42)
    //    required: com.us.dsb.explore.types.strong.ValueClassTypeSafetyExplTest.PoiId
  }


  val MOVING_TIME_THRESHOLD = MovingTime(45)
  val movingTime1 = MovingTime(30)
  val movingTime2 = MovingTime(60)
  val spNumber = SpNumber(30)


  test("NO strong typing for equality comparisons.") {

    // Allows correct type:
    assertResult(true)(movingTime1 == movingTime1)
    assertResult(false)(movingTime1 == movingTime2)

    // Fails to catch wrong type (worse, it always returns false).
    assertResult(false)(spNumber == movingTime1)
    assertResult(false)(spNumber == 30)              // !
    assertResult(false)(spNumber == spNumber.value)  // !
    // If warnings enabled:
    // "Warning:(...) comparing case class values of types ... SpNumber and
    //   MovingTime using `==' will always yield false
  }


  test("No operations from underlying type (sometimes good, sometimes not).") {

    // Does not ~inherit operations of underlying type:
    "movingTime1 + movingTime2" shouldNot compile  // (_does_ not)
    "movingTime1 * movingTime1" shouldNot compile  // (_does_ not)
    "movingTime1 < movingTime1" shouldNot compile  // (_does_ not)
    // "Error:(...) value < is not a member of ... MovingTime"
  }


  test("Need explicit unwrapping/wrapping for operations (except ==).") {

    movingTime1.value + movingTime2.value
    movingTime1.value * movingTime1.value
    movingTime1.value < movingTime1.value

    assertResult(true)(movingTime1.value == movingTime1.value)
    assertResult(false)(movingTime1.value == movingTime2.value)

    val movingTime3: MovingTime = MovingTime(movingTime1.value + movingTime2.value)

    assertResult(true)(spNumber.value == 30)
    assertResult(false)(spNumber.value == 31)
  }

  /* NOTE: assertDoesNotCompile doesn't work (on this) for 2.10.
  test("(Can't be local classes.) [assertDoesNotCompile doesn't work for 2.10]") {
    assertDoesNotCompile("case class MovingTime1(value: Int) extends AnyVal")
    "case class MovingTime2(value: Int) extends AnyVal" shouldNot compile  // (_does_ not)
    // "Error:(...) value class may not be a local class
  }
  */


  test("(Can't have constructor (statements).)") {
    // See comment in MovingTime.
  }

  test("(Can't be subclassed.)") {
     // Value class is final.  Also, as case class, can't extend another case class.
  }

}
