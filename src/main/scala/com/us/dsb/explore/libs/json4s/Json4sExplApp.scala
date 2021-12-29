package com.us.dsb.explore.libs.json4s

import org.json4s.{DateFormat, DefaultFormats, FieldSerializer, Formats, JValue}
import org.json4s.JsonAST.{JField, JObject}
//import org.json4s.jackson.JsonMethods
import org.json4s.native.JsonMethods


//import org.json4s._
//import org.json4s.jackson.JsonMethods
//import org.json4s.jackson.JsonMethods._
//import org.json4s.JsonDSL._ // adds lots of implicits such as seq2jvalue(...)
// import org.json4s.native.Serialization._
// import org.json4s.jackson.Serialization._
// import org.json4s.Xml...

/*
 * NOTES:
 *
 * Transformation terms/steps:
 * - JSON string to JValue tree - parse
 * - JValue tree to JSON string - Jackson render, then compact or pretty
 * - JValue tree to object tree - extract
 * - object tree to JValue tree - decompose???
 * - object tree to JSON string - serialize (?)
 * - JSON string to object tree - deserialize (?)
 *
 *
 *
 * org.json4s.JsonMethods[T] defines methods parse/parseOpt, render, compact/pretty
 * - T is output of render (not parse/parseOpt) and input of compact/pretty
 * org.json4s.native.JsonMethods extends JsonMethods[Document]
 * org.json4s.jackson.JsonMethods extends JsonMethods[JValue]
 */


/* org.json4s package object:
 *
 * - JValue, JString, JInt, etc., type aliases and val ~aliases for JsonAST.*
 * - TypeInfo type alias and val ~alias for  reflect.TypeInfo


trait ParameterNameReader extends reflect.ParameterNameReader

implicit def string2JsonInput(s: String): JsonInput = StringInput(s)
implicit def reader2JsonInput(rdr: java.io.Reader): JsonInput = ReaderInput(rdr)
implicit def stream2JsonInput(stream: java.io.InputStream): JsonInput = StreamInput(stream)
implicit def file2JsonInput(file: java.io.File): JsonInput = FileInput(file)

implicit def jvalue2extractable(jv: JValue) = new ExtractableJsonAstNode(jv)
implicit def jvalue2monadic(jv: JValue) = new MonadicJValue(jv)
implicit def jsonwritable[T: Writer](a: T) = new ToJsonWritable[T](a)

case class MappingException
*/

/*
 * Queue:
 * - (parsing)
 * - generating JSON (JValues) from non-JValue data structures
 * - generating JSON (JValues) from with DSL ->, ~, etc.?)
 * - querying JSON (JValues) with DLS (\, \\, array indexing, ?)
 *   - merge?
 * - extracting objects from JSON (JValues)
 *   - non-custom (e.g., case classes)
 *   - DefaultFormats
 *   - custom objects (e.g., non-case classes)
 *   - custom property/field mappings
 * - "serializing"  (What exactly)
 *   - ...
 *   - customer serializer
 * -??
 */

object Json4sExplApp extends App {

  ///////////////////
  // Input:  Specific class objects
  {
    // ?? import org.json4s.jackson.Serialization

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
    // ?? val xmp51JsonStr2: String = Serialization.write(xmp51Objs)
    // ?? println("xmp51JsonStr2 = " + xmp51JsonStr2)

    /*

    Now the type WildDog (and all subtypes) gets serialized with all its fields (+ constructor parameters). FieldSerializer takes two optional parameters which can be used to intercept the field serialization:

    case class FieldSerializer[A: Manifest](
      serializer:   PartialFunction[(String, Any), Option[(String, Any)]] = Map(),
      deserializer: PartialFunction[JField, JField] = Map()
    )

    Those PartialFunctions are called just before a field is serialized or deserialized. Some useful PFs to rename and ignore fields are provided:

    val dogSerializer = FieldSerializer[WildDog](
      renameTo("name", "animalname") orElse ignore("owner"),
      renameFrom("animalname", "name"))

    implicit val formats = DefaultFormats + dogSerializer
     */

  }

  /* ???1


  ////////////////////
  // Input:  Actual JSON text.

  val xmp11JsonStr = """ { "i" : 1,  "s": "some string" }    """
  println("xmp11JsonStr = " + xmp11JsonStr)

  val xmp11Jvalue1: JValue = JsonMethods.parse(xmp11JsonStr)
  println("xmp11Jvalue1 = " + xmp11Jvalue1)

  val xmp11JsonStr2 = JsonMethods.pretty(xmp11Jvalue1)
  println("xmp11JsonStr2 = " + xmp11JsonStr2)


  ////////////////////
  // Input:  Generic scala objects (primitives and collection thereof).
  {
    import org.json4s.JsonDSL._  // impl. convs. from various types to JObject

    val xmp21GenObjs = 2
    println("xmp21GenObjs = " + xmp21GenObjs)
    val xmp21Jvalue = JsonMethods.render(xmp21GenObjs) // impl. int2jvalue
    println("xmp21Jvalue = " + xmp21Jvalue)

    val xmp22GenObjs = List(1, 2, 3)
    println("xmp21GenObjs = " + xmp21GenObjs)
    val xmp22Jvalue = JsonMethods.render(xmp21GenObjs) // impl. seq2jvalue
    println("xmp22Jvalue = " + xmp22Jvalue)

    val xmp23GenObjs1: Map[String, Int] = Map("A" -> 65, "B" -> 66)
    println("xmp23GenObjs1 = " + xmp23GenObjs1)
    val xmp23Jvalue2 = JsonMethods.render(xmp23GenObjs1) // impl. map2jvalue
    println("xmp23Jvalue2 = " + xmp23Jvalue2)
    
    // NOTE: Doesn't seem to handle Map of heterogeneous values:
    val xmp24GenObjs1 = Map("A" -> 65, "B" -> 1.1)
    println("xmp24GenObjs1 = " + xmp24GenObjs1)
    //val xmp24Jvalue1 = JsonMethods.render(map2jvalue(xmp24GenObjs1))
    //val xmp24Jvalue2 = JsonMethods.render(xmp24GenObjs1) // use JsonDSL's implicits
  }


  ////////////////////
  // Input:  DSL code.
  {
    import org.json4s.JsonDSL._

    val xmp31Jvalue: JObject = ("a" -> 65) ~ ("b" -> "bee")
    println("xmp31Jvalue = " + xmp31Jvalue)

    // NOTE:  Can't make one-property object without doing something else.
    val xmp32NotJvalue = ("a" -> 65)
    println("xmp32NotJvalue = " + xmp32NotJvalue)
    val xmp32Jvalue: JValue = JsonMethods.render(xmp32NotJvalue)
    println("xmp32Jvalue = " + xmp32Jvalue)


    // Note: Does not handle case classes automatically; need implicit conversion
    // methods.

    case class CC(x: Int)

    // import org.scalatest.Matchers._
    // "val xmp223Jvalue1: JObject = (\"a\" -> \"ay\") ~ (\"b\" -> CC(1))" shouldNot compile
    // "No implicit view available from CC => org.json4s.JsonAST.JValue"
    // "not enough arguments for method ~: (implicit ev1: CC => org.json4s.JsonAST.JValue)org.json4s.JsonAST.JObject.
    // Unspecified value parameter ev1."

    implicit def cc2Jvalue(cc: CC): JValue = { JObject(JField("x", cc.x)) }

    val xmp33Jvalue: JObject = ("a" -> "ay") ~ ("b" -> CC(1))
    println("xmp33Jvalue = " + xmp33Jvalue)
  }

  ////////////////////
  // Output:  Specific class objects (case and non-case).
  {
    val xmp41JsonStr = """ { "f1": "something", "f2": { "f3": 123} } """
    println("xmp41JsonStr = " + xmp41JsonStr)

    val xmp41Jvalue1: JValue = JsonMethods.parse(xmp41JsonStr)
    println("xmp41Jvalue1 = " + xmp41Jvalue1)


    case class Something(f1: String, f2: SomethingElse)
    case class SomethingElse(f3: Int)

    //implicit val formats = org.json4s.DefaultFormats
    implicit val formats: Formats = new Formats {
      override def dateFormat: DateFormat = ???
    }






    val xmp41Objs = xmp41Jvalue1.extract[Something]
    println("xmp41Objs = " + xmp41Objs)

    class Something2(val f1: String, val f2: SomethingElse2)
    class SomethingElse2(val f3: Int)

    val xmp41Objs2: Something2 = xmp41Jvalue1.extract[Something2]
    println("xmp41Objs2 = " + xmp41Objs2)
    println("xmp41Objs2.f1 = " + xmp41Objs2.f1)
    println("xmp41Objs2.f2 = " + xmp41Objs2.f2)
    println("xmp41Objs2.f2.f3 = " + xmp41Objs2.f2.f3)

  }



  //??? What is relationship between extract and read?


  //org.json4s.jackson.JsonMethods.extr



  // (same for org.json4s.native.JsonMethods.parse)

/*
  println("jvalue.productArity = " + jvalue1.productArity)
  assert(1 == jvalue1.productArity)
  for (i <- 0 until jvalue1.productArity) {
    println(s"jvalue.productElement($i) = " + jvalue1.productElement(i))
  }

  def dump(jvalue: JValue, indent: String = ""): Unit = {
    def x(a: Any) = Console.println(a)
    def println(a: Any) = x(indent + a)
    jvalue match {
      case JNull    => println(s"JNull()")
      case JBool(b)    => println(s"JBool( $b )")
      case JInt(i)     => println(s"JInt( $i )")
      case JDouble(d)  => println(s"JDouble( $d )")
      case JString(s)  => println("JInt( \"" + s + "\" )")
      case JArray(elems) =>
        println("JArray(")
        elems.foreach(e => dump(e, indent + " . "))
        println(")")
      case JObject(fields) =>
        println("JObject(")
        for (f <- fields) {
          f match {
            case (name, value) =>
              val name = f._1
              val value = f._2
              println("field " + f._1 + ":")
              dump(value, indent + " . ")
          }
        }
        println(")")
      case null =>
    }
  }
  dump(jvalue1)
*/

/*
  println("jvalue.values = " + jvalue1.values)
  println("jvalue.children = " + jvalue1.children)
  println("jvalue.toOption = " + jvalue1.toOption)
  println("jvalue.toSome = " + jvalue1.toSome)
  println("jvalue.apply(0) = " + jvalue1.apply(0))
  println("jvalue.apply(1) = " + jvalue1.apply(1))
  println("jvalue.apply(20) = " + jvalue1.apply(20)) // no index error for null, object


  val x2j: JValue = org.json4s.jackson.JsonMethods.render(jvalue1)
  val x2n: Document = org.json4s.native.JsonMethods.render(jvalue1)
  assert(x2j == jvalue1)
*/
  /*
  println("x2n = " + x2n)
  println("x2j = " + x2j)

  val x3jc: String = org.json4s.jackson.JsonMethods.compact(x2j)
  val x3nc: String = org.json4s.native.JsonMethods.compact(x2n)
  val x3jp: String = org.json4s.jackson.JsonMethods.pretty(x2j)
  val x3np: String = org.json4s.native.JsonMethods.pretty(x2n)

  println("x3jc = " + x3jc)
  println("x3nc = " + x3nc)
  println("x3jp = " + x3jp)
  println("x3np = " + x3np)
*/

  ???1 */
}
