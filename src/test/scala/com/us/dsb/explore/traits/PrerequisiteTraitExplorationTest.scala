package com.us.dsb.explore.traits

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers._


class PrerequisiteTraitExplorationTest extends AnyFunSuite {

  // Trait required by some other trait.
  trait RequiredTrait {

    def requiredMethod():String  = {
      try {
        //println("+ RequiredTrait . requiredMethod")
        "RequiredTrait.requiredMethod"
      }
      finally {
        //println("- RequiredTrait . requiredMethod")
      }
    }

    // abstract def method2():String  - "`abstract' modifier can be used only for classes; it should be omitted for abstract members"

    // override def method3():String  - "method method3 overrides nothing"
    // abstract override def method4():String - "method method4 overrides nothing"

  }

  // Trait requiring first trait, attempted with required trait in "extends"
  // clause (per text in question).
  trait RequiringTrait1
      extends
          RequiredTrait
  {

    def requiringMethod(): String = {
      try {
        //println("+ Requiring1Trait . method")
        requiredMethod() + " - " + "RequiringTrait1.requiringMethod"
      }
      finally {
        //println("- Requiring1Trait . requiringMethod")
      }
    }

  }


  // Trait requiring first trait, done using self type.
  trait RequiringTrait2 {
    self: RequiredTrait =>

    // abstract override def detect() = super.detect() "value detect is not a member of AnyRef"
    def requiringMethod(): String = {
      //println("+ Requiring2Trait . requiringMethod")
      try {
        self.requiredMethod() + " - " + "RequiringTrait2.self.requiringMethod (1)"
        //works too: requiredMethod() + " - " + "RequiringTrait2.requiringMethod (2)"
      }
      finally {
        //println("- Requiring2Trait . requiringMethod")
      }
    }

  }


  test("xxx.") {

    // TODO:  To be explored further:  Somewhat unexpectedly, the required trait
    // can appear later in the CompoundType (after "extends") than the trait in
    // question does.  (How does that affect linearization and cross-trait
    // constructor and method call order?)

    class BaseClass

    class ClientClass1 extends BaseClass with RequiredTrait with RequiringTrait2
    {
      // abstract override def method() ... - "`abstract override' modifier only allowed for members of traits"
      // abstract def method() ... - "`abstract' modifier can be used only for classes; it should be omitted for abstract members"
      // def method() ... - "overriding method method in trait RequiredTrait of type ()String; method method needs `override' modifier"

      override def requiredMethod() = {
        try {
          //println("+ ClientClass1 . requiredMethod")
          super.requiredMethod() + " - " + "ClientClass1.requiredMethod"
        }
        finally {
          //println("- ClientClass1 . requiredMethod")
        }
      }

      override def requiringMethod() = {
        try {
          println("+ ClientClass1 . requiringMethod")
          super.requiringMethod() + " - " + "ClientClass1.super.requiringMethod"
        }
        finally {
          println("- ClientClass1 . requiringMethod")
        }
      }
    }

    /*
    class ClientClass2 extends BaseClass with RequiringTrait2 with RequiredTrait
    {
      // override def method() ...  - "overriding method method in trait RequiringTrait2 of type ()String; method method needs `abstract override' modifiers"
      // abstract override def method() ... - "`abstract override' modifier only allowed for members of traits"
      // abstract def method() ... - "`abstract' modifier can be used only for classes; it should be omitted for abstract members"
      // def method() ... - "overriding method method in trait RequiredTrait of type ()String; method method needs `override' modifier"

      // with no method method(), then, on class:
      //   "class ClientClass2 inherits conflicting members:
      //   method method in trait RequiringTrait2 of type ()String  and
      //   method method in trait RequiredTrait of type ()String
      //   (Note: this can be resolved by declaring an override in class ClientClass2.)"
      // Note that that note seems to be false in this case.

      /*
      def method() = {
        try {
          println("+ ClientClass2 . method")
          super.method() + " - " + "ClientClass2.method"
        }
        finally {
          println("+ ClientClass2 . method")
        }
      }
      */

    }

     */


    val x11 = new ClientClass1
    //val x12 = new ClientClass2
    val x21 = x11.requiringMethod()
    //val x22 = x12.method
    println("x21 = " + x21)
    //println("x22 = " + x22)
  }

}
