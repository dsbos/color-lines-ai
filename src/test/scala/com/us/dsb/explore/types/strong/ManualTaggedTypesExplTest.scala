package com.us.dsb.explore.types.strong

import org.scalatest.funsuite.AnyFunSuite
//import org.scalatest.matchers._
import org.scalatest.matchers.should.Matchers._  // for "shouldNot compile"


/**
  * Demo test of tagged types of <what?> form (old Scalaz form).
  */
class ManualTaggedTypesExplTest extends AnyFunSuite {

  test("Tagged types in old form should ...") {


    ////////////////////////////////////////
    // Feature-implementation code (base; see more below):

    /**
      * ~: Something that is tagged with a (tagging/semantic) type.
      * (Type constructor that makes type with type member named Tag that is
      * alias for type parameter.)
      * @tparam  U  the type that tags the thing
      */
    type Tagged[U] = {
      /**
        * The tagging type.
        * (Type alias.)
        * (Name is not used anywhere.)
        */
      type Tag = U
    }

    /**
      * Something that is of some (data) type tagged with some (semantic) type.
      * (Type constructor that makes type that is (subclass of) the data type
      * and is (~subclass of) type with type member Tag that is alias of the
      * semantic type, and that type-erases to the data type.)
      * @tparam  T  the data type
      * @tparam  U  the tagging type
      */
    type @@[T, U] = T with Tagged[U]


    ////////////////////////////////////////
    // Feature-use code (part 1):

    ////////////////////
    // Tagged-type declarations:

    // Semantic types for kinds of things.
    trait ThingA
    trait ThingB

    // Representation types (tagged data types) of kinds of things.
    // (Subclass of Int; ~subclass of ~tag-attaching type with respective
    // tagging type; erases to Int.)
    type ThingAInt = Int @@ ThingA
    type ThingBInt = Int @@ ThingB
    // (Could simultaneously have "type ThingAString = String @@ ThingA".)

    ////////////////////
    // Strong typing at assignment and parameter association:

    val a0: ThingAInt = 1.asInstanceOf[ThingAInt]
    val b0: ThingBInt = 2.asInstanceOf[ThingBInt]

    val a1: ThingAInt = a0
    //val a2: ThingAInt = b0
    " val a2: ThingAInt = b0" shouldNot typeCheck
    //val a3: ThingAInt = 1
    " val a3: ThingAInt = 1" shouldNot typeCheck

    def f(a: ThingAInt) = ()
    f(a0)
    //f(b0)
    " f(b0) " shouldNot typeCheck
    //f(1)
    " f(1) " shouldNot typeCheck

    ////////////////////
    // Tagged-type expr. can be used as data-type expr., but methods do not propagate type:
    val i1: Int = a0
    1 + a0
    a0 + a0
    //val a4: ThingAInt = a0 + a0  // type is only Int, not ThingAInt
    " val a4: ThingAInt = a0 + a0" shouldNot typeCheck


    ////////////////////
    // Weak typing elsewhere (because of substitutability just above):
    if (a0 > b0) {}
    a0 + b0          // and type is Int, not ThingAInt


    ////////////////////////////////////////
    // Feature-implementation code (part 2):

    // For reducing ".asInstanceOf[...]" syntax:

    class Tagger[U] {  //????? REVISIT re "object" in Scalaz
      def apply[T](t: T): T @@ U = t.asInstanceOf[T @@ U]
    }
    def tag[U] = new Tagger[U]  //???? REVISIT re allocating objects

    ////////////////////////////////////////
    // Feature-use code (part 2):

    val a11: ThingAInt = 11.asInstanceOf[ThingAInt]

    val a12 = tag[ThingA](12)     // IDE says type is @@[Int, ThingA] (~= ThingAInt)
    val a13: ThingAInt = a12

    // Scala 2.10 compiler bug?:
    //val t1: ThingAInt = tag[ThingA](12)       // "type mismatch ... Int(12) ... Int"! Huh?
    //val t2: ThingAInt = tag[ThingA](12: Int)  // "type mismatch ... Int ... Int"! Huh?
    //val t3: ThingAInt = tag[ThingA](1 + 1)    // "type mismatch ... Int(2) ... Int"! Huh?

    // Try:
    // def tag2[Repr, U](v: Repr): Repr @@ U = v.asInstanceOf[Repr @@ U]


    // "Error: type mismatch; found   : Int; required: Int" - Huh?:
    //val a21: ThingAInt = tag(a0 + a0)
    //
    // "Error: type mismatch; found   : Int; required: Int" - Huh?:
    //val a21: ThingAInt = tag[ThingA](a0 + a0)

    val a22:    Int          = tag[ThingA](12)
    val a23: @@[Int, ThingA] = tag[ThingA](12)
    val x1: Int = a22
    val x2: Tagged[ThingA] = a23

    // "Error: ... type mismatch; found   : Int(12); required: Int" - Huh?:
    // val a24: Int with Tagged[ThingA] = tag[ThingA](12)

    val z1 = 12
  }

}
