package com.us.dsb.explore.libs.scalatest

import org.scalatest.funspec.AnyFunSpec

class ScratchTest extends AnyFunSpec  {

  // https://www.scalatest.org/user_guide/using_matchers#expectedExceptions
  // https://www.scalatest.org/user_guide/using_matchers#usingCustomMatchers
  // https://www.scalatest.org/user_guide/using_matchers#logicalExpressions
  // https://www.scalatest.org/user_guide/using_matchers#checkingThatCodeDoesNotCompile
  // https://www.scalatest.org/user_guide/using_matchers#inspectorShorthands

  it("EitherValues") {
    val right: Either[String, Int] = Right(1)

    /* Without EitherValues, left/right failure gets simple "Either.left.get on Right"
       with test line number down a bit in stack (but still linked in IntelliJ):
         Either.left.get on Right
         java.util.NoSuchElementException: Either.left.get on Right
           at scala.util.Either$LeftProjection.get(Either.scala:537)
           at com.us.dsb.explore.libs.scalatest.ScratchTest.$anonfun$new$1(ScratchTest.scala:10)
           at ...
     */
    //assert("123" == right.left.get)
    assert(1 == right.right.get)

    import org.scalatest.EitherValues._

    /* With EitherValues, gets display of actual value and test line number up
       top:

         The Either on which left.value was invoked was not defined as a Left; it was Right(1).
         ScalaTestFailureLocation: com.us.dsb.explore.libs.scalatest.ScratchTest at (ScratchTest.scala:22)
         org.scalatest.exceptions.TestFailedException: ...

     */
    //assert("123" == right.left.value)
  }

  ignore("OptionValues") {
  }
  ignore("PartialFunctionValues") {
  }
  ignore("Inside? matchPattern?") {
  }
  ignore("loneElement") {
  }

}
