package com.us.dsb.explore.strongtypes


// https://github.com/estatico/scala-newtype
import io.estatico.newtype.macros._


object BasicNewtypes extends App {
  println("""See BasicNewtypesExplTest ('"..." shouldNot typeCheck' cases).""")

  object Types {
    // Quiet warning "implicit conversion method opsThis should be enabled":
    import scala.language.implicitConversions

    // (Must be in object, since macro defines a type.)
    @newtype case class Primitive(raw: Int)

    @newtype case class SameShapePrimitive(raw: Int)

    @newtype case class Primitive2(raw: Int) {
      def get: Int = {
        this.raw
      }
    }

    @newtype case class Nested(raw: Primitive)

    @newtype case class Deeper(raw: List[Nested])
    @newtype case class SameShapeDeeper(raw: List[Nested])

    @newtype case class FlatSpecificCollection(raw: List[Int])

    @newtype case class FlatGenericCollection[A](raw: List[A])

    @newtype case class FlatGenericCollection2(raw: List[_])

  }
  import Types._

  var x1xx = Primitive(1)
  x1xx = Primitive(2)


  val x1a = x1xx
  val x2 = Nested(x1xx)
  val x3 = Deeper(List(x2, x2))
  val x4 = FlatSpecificCollection(List())
  val x6 = FlatSpecificCollection(List(1, 2))
  val x6b = FlatGenericCollection(List("", ""))
  val x6c = FlatGenericCollection2(List("", ""))
  x1xx.raw
  x2.raw
  x3.raw
  x4.raw

}
