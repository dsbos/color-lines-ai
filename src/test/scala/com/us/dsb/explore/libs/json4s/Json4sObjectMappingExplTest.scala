package com.us.dsb.explore.libs.json4s

import org.json4s.jackson.JsonMethods.compact
//import org.json4s.native.JsonMethods.compact
import org.json4s.jackson.Serialization
import org.json4s.{DefaultFormats, Extraction, FieldSerializer, JField}
import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.meta.param


class IntValueClass(val value1: Int) extends AnyVal
case class StringValueClass(value2: String) extends AnyVal
case class OptionValueClass(val value3: Option[String]) extends AnyVal

class IntNonvalueClass(val value1: Int)

//noinspection CaseClassParam
class Json4sObjectMappingExplTest extends AnyFunSuite {


  case class Tree1Root(val t1r_cp1: String, t1r_cp2: Tree1Child)
  case class Tree1Child(t1c_cp1: Int)

  test("Demo: Default ser. of case class gets constructor-parameter fields.") {
    val tree1Obj = Tree1Root("one", Tree1Child(4))
    println("tree1Obj = " + tree1Obj)

    implicit val formats = org.json4s.DefaultFormats
    val tree1Json: String = Serialization.write(tree1Obj)
    val tree1Json2: String = compact(Extraction.decompose(tree1Obj))
    println("tree1Json = " + tree1Json)
    println("tree1Json2 = " + tree1Json2)
    println()
    assert(tree1Json.contains("t1r_cp1"))
    assert(tree1Json.contains("t1r_cp2"))
    assert(tree1Json.contains("t1c_cp1"))
  }


  case class Tree2Root(val t2r_cp1: String, t2r_cp2: Tree2Child) {
                       var t2r_pubVar: Int = 123
               private var t2r_privVar: Float = 1.5f
               private val t2r_privVal: Long = 2L
                       def t2r_getter: Int = 42
  }
  case class Tree2Child(t2c_cp1: Int, @transient t2c_cp2: Int )

  val tree2Obj = Tree2Root("one", Tree2Child(4, 5))
  tree2Obj.t2r_pubVar = 3

  test("Demo: Default ser. of case class ignores non-constructor fields.") {
    println("tree2Obj = " + tree2Obj + " (not showing non-constructor fields)")

    implicit val formats = org.json4s.DefaultFormats
    val tree2Json: String = Serialization.write(tree2Obj)
    println("tree2Json = " + tree2Json)
    println()
    assert(  tree2Json.contains("t2r_cp1"))
    assert(  tree2Json.contains("t2r_cp2"))
    assert(! tree2Json.contains("t2r_pubVar"))
    assert(! tree2Json.contains("t2r_privVar"))
    assert(! tree2Json.contains("t2r_privVal"))
    assert(! tree2Json.contains("t2r_getter"))
  }

  test("Demo: FieldSerializer for case class adds getting non-constructor fields.") {
    println("tree2Obj = " + tree2Obj + " (not showing non-constructor fields)")

    val serializer: PartialFunction[(String, Any), Option[(String, Any)]] = {
      case tuple =>
        println(s"tuple: ${tuple._1}:${tuple._1.getClass.getSimpleName} -> ${tuple._2}: ${tuple._2.getClass.getSimpleName}")
        Some(tuple): Option[(String, Any)]
    }



    implicit val formats = DefaultFormats + FieldSerializer[Tree2Root](
      serializer = serializer,
      deserializer = null: PartialFunction[JField, JField] //????,
      //????includeLazyVal = false
    )

    val tree2Json: String = Serialization.write(tree2Obj)
    println("tree2Json = " + tree2Json)
    println()
    assert(  tree2Json.contains("t2r_cp1"))
    assert(  tree2Json.contains("t2r_cp2"))
    assert(  tree2Json.contains("t2r_pubVar"))
    assert(  tree2Json.contains("t2r_privVar"))
    assert(  tree2Json.contains("t2r_privVal"))
    assert(! tree2Json.contains("t2r_getter"))
  }


  class Tree3Root(val t3r_cp1: String, t3r_cp2: Tree3Child) {
    var t3r_pubVar: Int = 123
    private var t3r_privVar: Float = 1.5f
    private val t3r_privVal: Long = 2L
    def t3r_getter: Int = 42
  }
  class Tree3Child(t3c_cp1: Int, t3c_cp2: Int )

  val tree3Obj = new Tree3Root("one", new Tree3Child(4, 5))
  tree3Obj.t3r_pubVar = 3

  test("Demo: Default ser. of non-case class ignores non-constr.-param. fields (and non-field constr. params.).") {
    println("tree3Obj = " + tree3Obj)

    implicit val formats = org.json4s.DefaultFormats
    val tree3Json: String = Serialization.write(tree3Obj)
    println("tree3Json = " + tree3Json)
    println()
    assert(  tree3Json.contains("t3r_cp1"))
    assert(! tree3Json.contains("t3r_cp2"))
    assert(! tree3Json.contains("t3r_cp2"))
    assert(! tree3Json.contains("t3r_pubVar"))
    assert(! tree3Json.contains("t3r_privVar"))
    assert(! tree3Json.contains("t3r_privVal"))
    assert(! tree3Json.contains("t3r_getter"))
  }

  test("Demo: FieldSerializer for case class adds getting non-constructor-parameter fields.") {
    println("tree3Obj = " + tree3Obj + " (not showing non-constructor fields)")

    implicit val formats = DefaultFormats + FieldSerializer[Tree3Root]()

    val tree3Json: String = Serialization.write(tree3Obj)
    println("tree3Json = " + tree3Json)
    println()
    assert(  tree3Json.contains("t3r_cp1"))
    assert(! tree3Json.contains("t3r_cp2"))
    assert(  tree3Json.contains("t3r_pubVar"))
    assert(  tree3Json.contains("t3r_privVar"))
    assert(  tree3Json.contains("t3r_privVal"))
    assert(! tree3Json.contains("t3r_getter"))
  }

//?????

  case class Tree4Root(val t4r_cp1: IntValueClass,
                         val t4r_cp2: IntNonvalueClass,
                         var t4r_cp3: StringValueClass,
                         t4r_cp4: Tree4Child) {
    var t4r_pubVar: Int = 124
    private var t4r_privVar: Float = 1.5f
    private val t4r_privVal: Long = 2L
    def t4r_getter: Int = 42
  }
  class Tree4Child(t4c_cp1: Int, @(transient @param) t4c_cp2: Int )

  val tree4Obj = Tree4Root(new IntValueClass(1),
                           new IntNonvalueClass(2),
                           StringValueClass("three"), new Tree4Child(4, 5))
  tree4Obj.t4r_pubVar = 4

  test("??w/value classes: ??Demo: Default ser. of case class ignores non-constr.-param. fields (and non-field constr. params.).") {
    println("tree4Obj = " + tree4Obj)

    implicit val formats = org.json4s.DefaultFormats
    val tree4Json: String = Serialization.write(tree4Obj)
    println("tree4Json = " + tree4Json)
    println()
    assert(  tree4Json.contains("t4r_cp1"))
    assert(  tree4Json.contains("t4r_cp2"))
    assert(! tree4Json.contains(""""value1":1"""))
    assert(  tree4Json.contains(""""value1":2"""))
    assert(  tree4Json.contains("t4r_cp3"))
    assert(! tree4Json.contains("t4r_pubVar"))
    assert(! tree4Json.contains("t4r_privVar"))
    assert(! tree4Json.contains("t4r_privVal"))
    assert(! tree4Json.contains("t4r_getter"))
  }

  test("??w/value classes: ??Demo: FieldSerializer for non-case class adds getting non-constructor-parameter fields.") {
    println("tree4Obj = " + tree4Obj + " (not showing non-constructor fields)")

    implicit val formats = DefaultFormats + FieldSerializer[Tree4Root]()

    val tree4Json: String = Serialization.write(tree4Obj)
    println("tree4Json = " + tree4Json)
    println()
    assert(  tree4Json.contains("t4r_cp1"))
    assert(  tree4Json.contains("t4r_cp2"))
    assert(! tree4Json.contains(""""value1":1"""))
    assert(  tree4Json.contains(""""value1":2"""))
    assert(  tree4Json.contains("t4r_cp3"))
    assert(  tree4Json.contains("t4r_pubVar"))
    assert(  tree4Json.contains("t4r_privVar"))
    assert(  tree4Json.contains("t4r_privVal"))
    assert(! tree4Json.contains("t4r_getter"))
  }



  /*

implicit val formats = org.json4s.DefaultFormats
println("xmp51JsonStr = " + xmp51JsonStr)

val xmp51Objs2: Something = Serialization.read[Something](xmp51JsonStr)
println("xmp51Objs2 = " + xmp51Objs2)


JsonMethods.parse(xmp51JsonStr)



test("Default maps only properties/attributes from constructor parameters") {

//import org.json4s.jackson.Serialization




/*case*/ class Something(val f1: String, f2: SomethingElse) {
val f3: Int = 123
}
/*case*/ class SomethingElse(e3: Int)

val xmp51Objs = new Something("some string", new SomethingElse(321))
println("xmp51Objs = " + xmp51Objs)
/*

implicit val formats = org.json4s.DefaultFormats
val xmp51JsonStr: String = Serialization.write(xmp51Objs)
println("xmp51JsonStr = " + xmp51JsonStr)

val xmp51Objs2: Something = Serialization.read[Something](xmp51JsonStr)
println("xmp51Objs2 = " + xmp51Objs2)


JsonMethods.parse(xmp51JsonStr)

*/
// Renaming field/property (_inside_ given class, no:

//import FieldSerializer._
implicit val formats2 = DefaultFormats /*+
FieldSerializer[Something](renameTo("f3", "fThree"), renameFrom("f3", "3f")) +
FieldSerializer[SomethingElse](renameTo("e3", "eThree"), renameFrom("e3", "3e"))*/
val xmp51JsonStr2: String = Serialization.write(xmp51Objs)
println("xmp51JsonStr2 = " + xmp51JsonStr2)

*/


}
