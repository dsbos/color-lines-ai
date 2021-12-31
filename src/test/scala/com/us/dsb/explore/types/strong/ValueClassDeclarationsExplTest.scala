package com.us.dsb.explore.types.strong

import org.scalatest.funsuite.AnyFunSuite


// FOR A VALUE CLASS:

// Cannot be a member of another class (except class of singleton object).

// Can be member of a singleton object (or package (or package object)):
object SomeObject {
  case class SomeValueClass(value: Int) extends AnyVal
}


// "value class needs to have exactly one public val parameter"

// Can wrap primitive or object type:
case class Minimal1(value: Int   ) extends AnyVal
case class Minimal2(value: String) extends AnyVal

// Can (uselessly?) wrap Unit and Nothing:
case class Minimal3(value: Unit   ) extends AnyVal
case class Minimal4(value: Nothing) extends AnyVal

// Can't wrap Any, AnyRef/Object, or AnyVal:
// "Error:(17, 12) bridge generated for member method apply: (value: AnyVal)com.us.dsb.explore.types.strong.Minimal5 in object Minimal5
//   which overrides method apply: (v1: T1)R in trait Function1
//   clashes with definition of the member itself;
//   both have erased type (v1: Object)Object":
// case class Minimal5(value: AnyVal ) extends AnyVal
// case class Minimal6(value: AnyRef ) extends AnyVal
// case class Minimal7(value: Any    ) extends AnyVal
// case class Minimal7(value: Object  ) extends AnyVal

// Can't wrap another value class:
//   "Error: value class may not wrap another user-defined value class":
//   case class Minimal8(value: Minimal1) extends AnyVal

case class Try0a(value: String) extends AnyVal
// Can't extend another value class (because they're final):
// case class Try0b(value: String) extends Try0a

// Can't also extend a trait:
// "Error: illegal inheritance; superclass AnyVal
//  is not a subclass of the superclass Object
//  of the mixin trait SomeTrait":
// trait SomeTrait
// case class Try0c(value: String) extends AnyVal with SomeTrait


// Can have class body (but limited):
case class Try1(value: String) extends AnyVal {
}

// Cannot have body statements ("this statement is not allowed in value class").
// Cannot have instance fields ("field definition is not allowed in value class").
// Cannot have secondary constructors ("secondary constructor is not allowed in
//   value class").

// Can have instance methods:
case class Try4(value: String) extends AnyVal {
  def m1(): Unit = println("value = " + value)
  protected def m2() = value  // (though class can't be extended)
  private def m2(x: Int) = value + x
}

// Can have private constructor--but companion's apply(...) is still public:
package scope1 {
  case class Try5a private (value: String) extends AnyVal
  case class Try5b private[strong] (value: String) extends AnyVal
  // "Error: ....Try5c  does not have a constructor":
  // case class Try5c private[this] (value: String) extends AnyVal
}
package object scope2 {
  // "constructor Try5a in class Try5a cannot be accessed in package package":
  // new scope1.Try5a("")
  scope1.Try5a("")
}


// Companion object can be explicit:
case class Try6 private(value: String) extends AnyVal
case object Try6

// Companion object can't have overridden default apply method:
case class Try7 private(value: String) extends AnyVal
case object Try7 {
  // "method apply is defined twice ...":
  // def apply(value: String): Try7 = new Try7(value)
}

// Companion object can have non-default apply methods:
case class Try8 private (value: String) extends AnyVal
case object Try8 {
  def apply(prefix: String, suffix: String): Try8 = new Try8(prefix + suffix)
}

// Companion object can have non-apply factory methods:
case class Try9 private(value: String) extends AnyVal
case object Try9 extends Object {
  def build(value: String): Try9 = {
    if (value == "valid") {
      new Try9(value)
    }
    else {
      sys.error("")
    }
  }
}

// Companion object can have most other things(?):
case class Try10 private(value: String) extends AnyVal
case object Try10 extends Object {
  val x = 1
  case class X(v: Int) extends AnyVal
}


class ValueClassDeclarationsExplTest extends AnyFunSuite {

  test("") {
    new Try1("")
    new Try4("")
    // new Try6("") - private
    // new Try7("") - private
    // new Try8("") - private
    // new Try9("") - private
    // new Try10("") - private

    Try1("")
    Try4("")
    Try4("someString").m1()
    Try6("")  // ????
    Try7("")  // ????
    Try8("whole")  // ????
    Try9("")  // ????

    Try8("a", "b")

    Try9.build("valid")
    Try10.X(0)
  }

}
