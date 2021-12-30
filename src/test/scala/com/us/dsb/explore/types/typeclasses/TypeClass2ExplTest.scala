package com.us.dsb.explore.types.typeclasses

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.implicitNotFound


class TypeClass2ExplTest extends AnyFunSuite {

  //////////
  // Type class:


  @implicitNotFound("No (unique) member of type class Dumpable in scope for ${T}.")
  trait Dumpable[T] {              // // ?? Dumpable or Dumper?
     def dump1(v: T): String
  }

  //////////
  // Use of type class?:

  def dump2[T](t: T)(implicit d: Dumpable[T]) = d.dump1(t)

  // Context-bounds syntax:
  def dump2b[T: Dumpable](t: T) = {
//?????    val temp = context()
    implicitly[Dumpable[T]].dump1(t)
  }

  case class ThingOne(d: Double)
  case class ThingTwo(s: String, i: Int)


  object Hider {

    object SomethingA {

      //////////
      // A type class instance of Dumpable for ThingThingOne?:
      // Or: thing making ThingOne a member/instead of the type class?

      implicit object ThingOneDumper extends Dumpable[ThingOne] {
        def dump1(t: ThingOne): String = {
          t.toString
        }
      }
    }
    object SomethingC {
      implicit object ThingOneSpecialDumper extends Dumpable[ThingOne] {
        def dump1(t: ThingOne): String = {
          s"ThingOne(d = ${t.d})"
        }
      }
    }

    object SomethingB {
      implicit object ThingTwoDumper extends Dumpable[ThingTwo] {
        def dump1(t: ThingTwo): String = {
          t.toString
        }
      }
    }

  }


  val x1 = ThingOne(1)
  val x2 = ThingOne(1)
  val x3 = ThingTwo("", 1)


  {
    import Hider.SomethingA._
    println("x1 . dump2 . SomethingA: " + dump2(x1))
  }
  {
    import Hider.SomethingC._
    println("x1 . dump2 . SomethingC: " + dump2(x1))
  }

  {
    import Hider.SomethingA._
    println("x2 . dump2 . SomethingA: " + dump2(x2))
    dump2(x2)
  }

  {
    import Hider.SomethingB._
    println("x3 . dump2 . SomethingB: " + dump2(x3))
    dump2(x3)
  }

  //x.isValid



  // validation type class
  // validator instance form 1
  // validator instance


   test("") {

   }

}
