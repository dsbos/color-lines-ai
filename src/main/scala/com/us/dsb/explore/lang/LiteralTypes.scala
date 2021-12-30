package com.us.dsb.explore.lang

// https://docs.scala-lang.org/sips/42.type.html
object LiteralTypes extends App {

  var five: 5 = 5
  //five = 4
  //five = 4: 5
  five = 4.asInstanceOf[5]

  type Fives = List[5]
  var four = 4
  val fives1: Fives = List(5, 5, 5)
  //val fives2: Fives = List(5, 5, 5, four + 1)


  object AsTypeParameter {

    // Accepts Some(42) or None, but no other Some(<int>)
    def slightlyUseful(p: Option[42]): Unit = {
      ???
    }

    slightlyUseful(None)
    slightlyUseful(Some(42))
    //slightlyUseful(Some(123))
  }


  object CovariantReturn {

    trait T {
      def compute: Int
    }

    object One extends T {
      // "One" narrows compute's return type from "Int" to "1":
      override def compute: 1 = 1
    }

    object Two extends T {
      override def compute: 2 = 2
    }

    def acceptOne(x: 1) = ???


    val t = One: T

    // "One.compute" is of type "1", but related inferred types are "Int" (and
    //   IntelliJ doesn't understand right):

    val xInt: Int = One.compute
    val x1: 1 = One.compute
    //val x2: 2     = One.compute
    val xInferred = One.compute // infers :Int from :1
    final val xInferredFinal = One.compute // "final val" keeps :1

    acceptOne(1)
    acceptOne(One.compute)
    //acceptOne(t.compute)
    //acceptOne(xInt)
    acceptOne(x1)
    //acceptOne(xInferred)  // found: ... Int, required: 1
    acceptOne(xInferredFinal)

    def mInferred = One.compute
    //acceptOne(mInferred)  // found: Int, required: 1

    final def mInferredFinal = One.compute // but "final def" still infers :Int from :1
    //acceptOne(mInferredFinal)

    /*
     ASSIM. (from https://docs.scala-lang.org/sips/42.type.html):

     def foo[T](t: T): t.type = t
     final val bar = foo(23)            // result is bar: 23


     The presence of an upper bound of Singleton on a formal type parameter indicates that singleton types should be
     inferred for type parameters at call sites

    ~:
    (123: Any) match {
      case one: 1 => acceptOne(one)
      //case two: 2 => acceptOne(two)
    }

    */

  }

  valueOf[123]
  //valueOf[123.type]
  valueOf[LiteralTypes.type]
  //valueOf[LiteralTypes]

  // valueOf[Int]  no ValueOf Instance for non-singleton type

  val x1: 1 = 1
  val x2: Int = 1
  val x3 = List[Float]()
  valueOf[1]
  // valueOf[Int]  // error (expected)--no ValueOf Instance for non-singleton type
  valueOf[x1.type]
  valueOf[x2.type] // ?? why not error?  why not doing "valueOf[Int]"?
  valueOf[x3.type] // ?? why not error?    MAYBE singleton vs. literal?

  //??def m[T <: Int with Singleton] : T = {
  //??  valueOf[T]
  //??}
  //??m[1]
  //m[""]

}
