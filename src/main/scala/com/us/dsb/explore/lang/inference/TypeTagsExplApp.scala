package com.us.dsb.explore.lang.inference

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._


object TypeTagsExplApp {

  class GeneralThing
  case class SpecificThingOne() extends GeneralThing
  case class SpecificThingTwo() extends GeneralThing {
    def someMethod(): Unit = {}
  }

  val things: List[GeneralThing] = List(SpecificThingOne(), SpecificThingTwo())

  import System.err.println
  def getThingViaTags[C <: GeneralThing](implicit classTag: ClassTag[C],
                                                  typeTag: TypeTag[C]): Option[C] = {
    println()
    println(s"classTag = $classTag (${classTag.getClass})")
    println(s"typeTag = $typeTag (${typeTag.getClass})")
    println(s"classTag.runtimeClass = $classTag.runtimeClass (${classTag.runtimeClass.getClass})")
    println(s"typeTag.tpe = $typeTag.tpe (${typeTag.tpe.getClass})")

    //val x1 = implicitly
    //val x2 = implicitly[TypeTag[_]]
    //val x2b = implicitly[TypeTag[C]]
    //val x3 = implicitly[C]
    val mirror = typeTag.mirror
    //println("typeTag.mirror = " + mirror)


    val class1 = classTag.runtimeClass
    println(s"class1 = $class1 (${class1.getClass})")

    val class2 = mirror.runtimeClass(typeTag.tpe)
    println(s"class2 = $class2 (${class2.getClass})")

    val class3 = mirror.runtimeClass(typeTag.tpe.typeSymbol.asClass)
    println(s"class3 = $class3 (${class3.getClass})")

    val class4 = mirror.runtimeClass(typeTag.tpe)
    println(s"class4 = $class4 (${class4.getClass})")

    assert(class1 == class2)
    assert(class2 == class3)
    assert(class3 == class4)

    val anyFirstMatch /*: Option[GeneralThing]*/ = things.find(obj => class1.isInstance(obj))
    println(s"anyFirstMatch = $anyFirstMatch (${anyFirstMatch.getClass})")

    anyFirstMatch.map(o => {
      val r1 /*: C*/ = o.asInstanceOf[C]  // (Only casts to GeneralThing, narrowest known runtime type for C.)
      val r2 /*: Any*/ = class1.cast(o)   // (Does cast to C (type in tags from call site).)
      r2.asInstanceOf[C]
    })
  }


  def main(args : Array[String]): Unit = {

    // As expected/wanted, passed tags are for SpecificThingOne:
    val v1 /*: Option[SpecificThingOne]*/ = getThingViaTags[SpecificThingOne]
    println("v1 = " + v1)

    // Not as expected/wanted, passed tags are for Nothing:
    val v2: Option[SpecificThingOne] = getThingViaTags
    println("v2 = " + v2)
    // Why is getThingViaTags's C inferred to be type Nothing?
    // Does type inference ever propagate the declared type _from_ a type
    // ascription in a variable declaration "down" _to_ the initialization
    // expression?  If it does, why doesn't it do so here?

    // Not as expected/wanted, class tags are for Nothing:
    def takesSpecificType(x: Option[SpecificThingTwo]): Unit = {}
    val v3 = takesSpecificType(getThingViaTags)
    println("v3 = " + v3)

    // Doesn't compile
    // getThingViaClassTag.get.someMethod()
    // Does type required/implied by use of an expression (SpecificThingTwo
    // implied by .someMethod())) not propagate down to interpretation of that
    // expression (getThingViaClassTag.get)?
  }

}
