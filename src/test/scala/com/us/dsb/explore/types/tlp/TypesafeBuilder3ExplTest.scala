package com.us.dsb.explore.types.tlp

import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.language.higherKinds


/**
  * Type-level example:  Type-safe range builder 3 - start and ((type-level) end or length).
  */
class TypesafeBuilder3ExplTest extends FunSuite {


  //////////
  // Type-level boolean type, true/false values, and logic functions (methods?):

  // Type-level sum(?) type:
  sealed trait BoolType {
    // Type-level abstract method Bool.Select(valueForTrue, valueForFalse) (to select
    // different type-level value based on ... equivalent of "this"):
    type Select[valIfTrue <: BoolType, valIfFalse <: BoolType] <: BoolType
  }

  // Type-level true value (or subtype?):
  sealed trait BoolTrue  extends BoolType { // Why sealed?  Does it need to be trait?
    // Type-level impl. for "true" of method If(valueForTrue, valueForFalse)
    type Select[valIfTrue <: BoolType, valIfFalse <: BoolType] = valIfTrue
  }
  // Type-level false value (or subtype?):
  sealed trait BoolFalse extends BoolType { // Why sealed?  Does it need to be trait?
    // Type-level impl. for "false" of method If(valueForTrue, valueForFalse)
    type Select[valIfTrue <: BoolType, valIfFalse <: BoolType] = valIfFalse
  }

  // Type-level functions:
  // - Not(a: Bool): Bool,         in terms of a.Select(false, true)
  // - &&(a: Bool, b: Bool): Bool, in terms of a.Select(b,     false)
  // - ||(a: Bool, b: Bool): Bool, in terms of a.Select(true,  b)

  type Not[A <: BoolType] = A#Select[BoolFalse, BoolTrue]
  type &&[A <: BoolType, B <: BoolType] = A#Select[B, BoolFalse]
  type ||[A <: BoolType, B <: BoolType] = A#Select[BoolTrue, B]

  // Unit tests for type-level boolean type/operations:

  test("Test BoolType stuff") {
    implicitly[BoolTrue =:= BoolTrue]
    implicitly[BoolFalse =:= BoolFalse]

    "implicitly[BoolTrue =:= BoolFalse]" shouldNot typeCheck
    assertTypeError("implicitly[BoolTrue =:= BoolFalse]")


    implicitly[Not[BoolTrue] =:= BoolFalse]
    implicitly[Not[BoolFalse] =:= BoolTrue]
    "implicitly[Not[BoolFalse] =:= BoolFalse]" shouldNot typeCheck

    implicitly[BoolFalse || BoolFalse =:= BoolFalse]
    implicitly[BoolFalse || BoolTrue =:= BoolTrue]
    implicitly[BoolTrue || BoolFalse =:= BoolTrue]
    implicitly[BoolTrue || BoolTrue =:= BoolTrue]

    implicitly[BoolFalse && BoolFalse =:= BoolFalse]
    implicitly[BoolFalse && BoolTrue =:= BoolFalse]
    implicitly[BoolTrue && BoolFalse =:= BoolFalse]
    implicitly[BoolTrue && BoolTrue =:= BoolTrue]

    implicitly[Not[BoolFalse && BoolTrue] =:= BoolTrue]
    implicitly[BoolTrue && Not[BoolTrue] =:= BoolFalse]
  }


  //////////
  // Type-safe builder


  // (make more private)
  case class Range(start: Double, end: Double/*, length: Double*/) {
    assert(end - start == length)
    def length = end - start
  }


  // Type-level class(type?) for state of builder.
  sealed trait BuilderState {
    type IsStartSet <: BoolType
    type IsLengthSet <: BoolType
    type IsEndSet <: BoolType
  }


  // Type-safe (value-level) builder - main (non-factory) part:

  case class RangeBuilder[S <: BuilderState] private (start:  Option[Double] = None,
                                                      end:    Option[Double] = None,
                                                      length: Option[Double] = None) {
    // (Can set start iff not already set.)
    def withStart(start: Double)
                 (implicit state: S#IsStartSet =:= BoolFalse
                 ): RangeBuilder[S {type IsStartSet = BoolTrue}] = {
      // (Don't need to check at value level whether start is already set.)
      this.copy(start = Some(start))
    }

    // (Can set end iff neither end nor length already set.)
    def withEnd(end: Double)
               (implicit state: S#IsEndSet || S#IsLengthSet =:= BoolFalse
               ): RangeBuilder[S {type IsEndSet = BoolTrue}] = {
      this.copy(end = Some(end))
    }

    // (Can set length iff neither length nor end already set.)
    def withLength(length: Double)
                  (implicit state: S#IsLengthSet || S#IsEndSet =:= BoolFalse
                  ): RangeBuilder[S {type IsLengthSet = BoolTrue}] = {
      this.copy(length = Some(length))
    }

    // (Can build iff both start and (end or length) are set.)
    def build()
             (implicit state: S#IsStartSet && (S#IsLengthSet || S#IsEndSet) =:= BoolTrue
             ): Range = {
      val netEnd = end.getOrElse(start.get + length.get)
      Range(start.get, netEnd)
    }
  }

  // Type-safe (value-level) builder - factory part:
  object RangeBuilder {

    // (Sets type-level state (return type) to same as value-level state:
    // neither start nor end is set.
    def apply() =
      new RangeBuilder[
        // Type-level instantiation of type-level class with type-level members
        // set to type-level initial boolean values:
        BuilderState {
          type IsStartSet = BoolFalse
          type IsLengthSet = BoolFalse
          type IsEndSet = BoolFalse
        }
        ]
  }


  test("Test good: given start and end") {
    val r1 = RangeBuilder().withStart(1.0).withEnd(10.0).build
    assert(((1.0, 9.0, 10.0)) == ((r1.start, r1.length, r1.end)))
  }
  test("Test good: given end and THEN start") {
    val r1 = RangeBuilder().withEnd(10.0).withStart(1.0).build
    assert(((1.0, 9.0, 10.0)) == ((r1.start, r1.length, r1.end)))
  }

  test("Test good: given start and length") {
    val r1 = RangeBuilder().withStart(1.0).withLength(9.0).build
    assert(((1.0, 9.0, 10.0)) == ((r1.start, r1.length, r1.end)))
  }
  test("Test good: given length and THEN start") {
    val r1 = RangeBuilder().withLength(9.0).withStart(1.0).build
    assert(((1.0, 9.0, 10.0)) == ((r1.start, r1.length, r1.end)))
  }

  test("Test bad: given length and end and no start") {
    assertTypeError("RangeBuilder().withLength(9.0).withEnd(10.0).build")
  }
  test("Test bad: given both end and length (no start)") {
    assertTypeError("RangeBuilder().withEnd(10.0).withLength(9.0).build")
  }
  test("Test bad: given both end and length (and start)") {
    assertTypeError("RangeBuilder().withStart(1).withEnd(10.0).withLength(9.0).build")
  }

  test("Test bad: given nothing") {
    assertTypeError("RangeBuilder().build")
  }

  test("Test bad: given only start") {
    assertTypeError("RangeBuilder().withStart(1.0).build")
  }
  test("Test bad: given only length") {
    assertTypeError("RangeBuilder().withLength(9.0).build")
  }
  test("Test bad: given only end") {
    assertTypeError("RangeBuilder().withEnd(10.0).build")
  }

  test("Test bad: given extra: start, start, end (full chain)") {
    assertTypeError("RangeBuilder().withStart(1.0).withStart(1.0).withEnd(10.0).build")
  }

  test("Test bad: given extra: start, start (fail-fast short chain)") {
    RangeBuilder()
    RangeBuilder().withStart(1.0)
    assertTypeError("RangeBuilder().withStart(1.0).withStart(1.0)")
  }

}


