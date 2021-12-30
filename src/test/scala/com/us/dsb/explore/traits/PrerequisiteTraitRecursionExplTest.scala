package com.us.dsb.explore.traits

import org.scalatest.funsuite.AnyFunSuite
//import org.scalatest.matchers._
import org.scalatest.matchers.should.Matchers._  // for "shouldNot compile"


/**
  * Demo test showing two mutually dependent traits.
  */
class PrerequisiteTraitRecursionExplTest extends AnyFunSuite {

  test("Demo mutual dependency between traits.") {

    // 1.  Pair of mutually dependent (concrete) traits:

    trait Trait1 {
      self: Trait2 =>

      def method1(i: Int): String = if (i >= 0) "method1" else self.method2(i)
    }

    trait Trait2 {
      self: Trait1 =>

      def method2(i: Int): String = if (i <= 0) "method2" else self.method1(i)
    }

    // 2.  Class can mix in both mutually dependent traits:

    class ClientClass12 extends Trait1 with Trait2

    // 3.  Class can't mix in only one of the mutually dependent traits:

    "class ClientClass1 extends Trait1" shouldNot compile
    // "illegal inheritance;
    //  self-type ...ClientClass1 does not conform to ...Trait1's selftype
    // ...Trait1 with ...Trait2


    // 4a.  Trait cannot simply extend just one of the mutually dependent traits
    // (without preserving the declaration of or satisfying the dependency on
    // the other):

    "trait Trait1B extends Trait1" shouldNot compile
    // "illegal inheritance;
    //  self-type Trait1B does not conform to Trait1's selftype Trait1 with Trait2"


    // 4b.  Trait can extend one of the mutually dependent traits if it preserves
    // the declaration of the dependency on the other, or if is satisfies that
    // dependency on the other:

    trait Trait2ModA extends Trait2 {
      self: Trait1 =>

      override def method2(i: Int) = super.method2(i) + " (...2 2ModA)"
    }

    trait Trait1And2ModB extends Trait2 with Trait1 {

      override def method1(i: Int) = super.method1(i) + " (...1 1And2ModB)"
      override def method2(i: Int) = super.method2(i) + " (...2 1And2ModB)"
    }

    // 4c.  Class can mix together extensions of the mutually dependent traits:
    class ClientClass2ModA extends Trait1 with Trait2ModA

    class ClientClass1And2ModB extends Trait1And2ModB


    // ...

    if (false) {
      val x1 = new ClientClass12
      println("method1( 1) = " + x1.method1( 1))
      println("method1( 0) = " + x1.method1( 0))
      println("method1(-1) = " + x1.method1(-1))
      println("method2( 1) = " + x1.method2( 1))
      println("method2( 0) = " + x1.method2( 0))
      println("method2(-1) = " + x1.method2(-1))
      println()
    }

    if (false) {
      val x1 = new ClientClass2ModA
      println("method1( 1) = " + x1.method1( 1))
      println("method1( 0) = " + x1.method1( 0))
      println("method1(-1) = " + x1.method1(-1))
      println("method2( 1) = " + x1.method2( 1))
      println("method2( 0) = " + x1.method2( 0))
      println("method2(-1) = " + x1.method2(-1))
      println()
    }


    if (false) {
      val x1 = new ClientClass1And2ModB
      println("method1( 1) = " + x1.method1( 1))
      println("method1( 0) = " + x1.method1( 0))
      println("method1(-1) = " + x1.method1(-1))
      println("method2( 1) = " + x1.method2( 1))
      println("method2( 0) = " + x1.method2( 0))
      println("method2(-1) = " + x1.method2(-1))
      println()
    }

  }

}
