package com.us.dsb.explore.traits

import org.scalatest.funsuite.AnyFunSuite
//import org.scalatest.matchers._
import org.scalatest.matchers.should.Matchers._  // for "shouldNot compile"


/**
  * Demo test to probe text on p. 234 of _Programming in Scala 3rd. ed._, that
  * says "... trait can only by mixed into a class that also extends [traits
  * mixed into that trait] ..."
  */
class PrerequisiteTraitDemoTest extends AnyFunSuite {

  // Trait to be required by some other trait.
  trait RequiredTrait

  // Trait attempting to require the required trait, by having the required
  // trait in "extends" clause (per book text and example in question).
  trait RequiringTrait1 extends RequiredTrait

  // Trait requiring required trait, by using self type.
  trait RequiringTrait2 {
    self: RequiredTrait =>
  }


  // 0. Confirm probing method:
  class DoesntHaveRequiredTrait
  class HasRequiredTraitAlready extends RequiredTrait

  "(null: DoesntHaveRequiredTrait): RequiredTrait" shouldNot typeCheck
  // Error has been:
  // "type mismatch; found   : ...DoesntHaveRequiredTrait
  //                 required: ...RequiredTrait"

  val x = (null: HasRequiredTraitAlready): RequiredTrait       // (compiles)


  // 1. Show that mixing in RequiringTrait1 does NOT actually require that a
  // class that mixes it in otherwise has RequiredTrait too (contrary to what
  // the text's wording implies) but actually adds RequiredTrait too (as
  // expected):
  test("Demo a requiring trait that also adds the required trait.") {
    class AddsRequiredTrait extends RequiringTrait1
    val x = (null: AddsRequiredTrait): RequiredTrait
  }

  // 2. Show that mixing in RequiringTrait2 DOES require that a class mixing
  // it in already/otherwise has RequiredTrait:
  test("Demo a requiring trait that really just requires the required trait.") {

    // declaring class with RequiringTrait2 but without RequiredTrait fails:
    "class DoesNotHaveOrAddRequiredTrait extends RequiringTrait2" shouldNot compile
    // Error has been:
    // "illegal inheritance; self-type DoesNotHaveOrAddRequiredTrait does not
    // conform to ...RequiringTrait2's selftype ...RequiringTrait2 with
    // ...RequiredTrait"

    // declaring class with RequiringTrait2 but with RequiredTrait works:
    class HasRequiredTraitOtherwise extends RequiredTrait with RequiringTrait2
    val x1 = (null: HasRequiredTraitOtherwise): RequiredTrait


    // TODO:  To be explored further:  Somewhat unexpectedly, the required trait
    // can appear later in the CompoundType (after "extends") than the trait in
    // question does.  (How does that affect linearization and cross-trait
    // constructor and method call order?)

    class ListsRequiredAfterRequiring extends RequiringTrait2 with RequiredTrait
    val x2 = (null: ListsRequiredAfterRequiring): RequiredTrait
  }

}
