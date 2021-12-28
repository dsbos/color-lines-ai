package com.us.dsb.explore.types.inference

import scala.reflect.{ClassTag => StandardClassTag}
import scala.annotation.unchecked.uncheckedVariance

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers._
import org.scalatest.matchers.should.Matchers._  // for "shouldNot compile"


/**
  * Demo of getting return type from context to be passed in through implicit
  * ClassTag parameter.
  */
class ContextInferredClasstagDemoTest extends AnyFunSuite {

  class BaseThing
  class SubthingOne() extends BaseThing
  class SubthingTwo() extends BaseThing

  // Key:  Need ClassTag's type parameter to be covariant (not invariant).
  type ModifiedClassTag[+T] = StandardClassTag[T @uncheckedVariance]

  def getPerTag[S <: BaseThing](implicit classTag: ModifiedClassTag[S]): (S, String) = {
    (null.asInstanceOf[S] /* dummy value of type */,
     classTag.runtimeClass.getSimpleName /* indicator of type */)
  }

  test("Demo getting return type taken from context.") {

    val v1: (SubthingOne, String) = getPerTag
    v1._2 shouldEqual "SubthingOne"

    def takesSpecificType(arg: (SubthingTwo, String)): String = {arg._2}
    val v2 = takesSpecificType(getPerTag)
    v2 shouldEqual "SubthingTwo"
  }

  test("Demo failure with standard ClassTag.") {

    def getPerTag[C <: BaseThing](implicit classTag: StandardClassTag[C]): (C, String) = {
      (null.asInstanceOf[C], classTag.runtimeClass.getSimpleName)
    }

    val v1: (SubthingOne, String) = getPerTag
    assert(v1._2 == "Nothing$")
  }

}
