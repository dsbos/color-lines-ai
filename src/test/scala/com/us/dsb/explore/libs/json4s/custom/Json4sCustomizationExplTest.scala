package com.us.dsb.explore.libs.json4s.custom

import com.fasterxml.jackson.databind.DeserializationConfig
import org.json4s.JsonAST._
import org.json4s.jackson.Serialization
import org.json4s.jackson.JsonMethods.compact
import org.json4s.native.JsonMethods.{compact => Native_compact}
import org.json4s.{CustomSerializer, DefaultFormats, Extraction, FieldSerializer, JValue, NoTypeHints}
import org.scalatest.funsuite.AnyFunSuite


/**
  * JSON4S notes:
  * - (Normally), fields from (primary) constructor parameters are serialized
  *   (per case-class vs. non-case-class/val/var rules).
  * - Normally, other fields are not serialized.
  * - With FieldSerializer, other fields are serialized.
  * - @Transient suppresses (some?) fields:
  *   - Case class constructor-parameter fields.
  *   - ?
  * - Value classes seem to work!  (I thought they didn't--check versions.)
  */


// Plain (nothing-special) wrapper classes:

/** Common base class for folding serializer cases. */
abstract class WrappedInt(val value: Int)

case class WrappedIntTime(override val value: Int) extends WrappedInt(value)
case class WrappedIntDist(override val value: Int) extends WrappedInt(value)

// Value classes:

case class IntValueClassTime(value: Int) extends AnyVal
case class IntValueClassDistance(value: Int) extends AnyVal


case class Child2(/*val wrappedTime: WrappedIntTime,
                   val wrappedDist: WrappedIntDist,
                   val valueTime: IntValueClassTime,
                   val optValueDist: Option[IntValueClassDistance],
                   @transient int_transient: Int*/)
case class Root2(val optValueDist: Option[IntValueClassDistance],
                 val optWrappedTime: Option[WrappedIntTime]/*,
                 val wrappedDist: WrappedIntDist,
                 val valueTime: IntValueClassTime,
                 var child: Child2*/)


//noinspection CaseClassParam
class Json4sObjectMappingExplTest extends AnyFunSuite {

  case class Child(val wrappedTime         : WrappedIntTime,
                   val wrappedDist         : WrappedIntDist,
                   val valueTime           : IntValueClassTime,
                   val optValueDist        : Option[IntValueClassDistance],
                   @transient int_transient: Int)
  case class Root(val wrappedTime : WrappedIntTime,
                  val wrappedDist : WrappedIntDist,
                  val valueTime   : IntValueClassTime,
                  val optValueDist: Option[IntValueClassDistance],
                  var child       : Child)

  test("Confirm that value classes are not parameter-assignment--compatible.") {
    import org.scalatest.matchers.should.Matchers._ // for "shouldNot compile"

    "val time1: WrappedIntTime = WrappedIntDist(0)" shouldNot compile
    "val time2: IntValueClassTime = IntValueClassDistance(0)" shouldNot compile
  }


  val tree4Obj1 = Root(WrappedIntTime(11),
                       WrappedIntDist(12),
                       IntValueClassTime(13),
                       Some(IntValueClassDistance(14)),
                       Child(WrappedIntTime(21),
                               WrappedIntDist(22),
                               IntValueClassTime(23),
                               Some(IntValueClassDistance(24)),
                               25))

  test("... FieldSerializer[Root] ...") {
    println("tree4Obj1 = " + tree4Obj1 + " (not showing non-constructor fields)")

    val serializer: PartialFunction[(String, Any), Option[(String, Any)]] = {
      case (propName, value) =>
        println(f"- propName: $propName%-10s, value: $value (${value.getClass})")

        value match {
          case x: WrappedInt =>   // ?? TODO: Does this ever match (re erasure)?
            Some((propName, x.value))
          case x =>
            Some((propName, x))
        }
    }

    implicit val formats =
      DefaultFormats +
      FieldSerializer[Root](serializer = serializer)

    val tree4Json: String = Serialization.write(tree4Obj1)
    //val tree4Json2 = compact(Extraction.decompose(tree4Obj))

    println("tree4Json = " + tree4Json)
    //println("tree4Json2 = " + tree4Json2)
    println()


    // Plain wrapper can be handled with custom field serializer on using class.
    assert(  tree4Json.contains(""""wrappedTime":11"""))
    assert(  tree4Json.contains(""""wrappedDist":12"""))

    // ~Direct use of value class (p: VC) appears as wrapped class.
    assert(  tree4Json.contains(""""valueTime":13"""))

    // ~Higher-order use of value class (e.g., p: Some[VC] .... ??
    assert(  tree4Json.contains(""""optValueDist":{"value":14}"""))

    // Plain wrapper, WITHOUT custom serializer on referencing class.
    assert(  tree4Json.contains(""""wrappedTime":{"value":21}"""))


    // @transient is honored.
    assert(! tree4Json.contains("""transient"""))


    //Serialization.read[Root](tree4Json)
  }



  val tree4Obj2 = Root2(Some(IntValueClassDistance(14)),
                        Some(WrappedIntTime(11))/*,
                        WrappedIntDist(12),
                        IntValueClassTime(13),
                        Child2(WrappedIntTime(21),
                               WrappedIntDist(22),
                               IntValueClassTime(23),
                               Some(IntValueClassDistance(24)),
                               25)*/)
  // Genericize:
  class WrappedIntTimeSerializer extends CustomSerializer[WrappedIntTime](format => (
      { case JInt(i)       => WrappedIntTime(i.intValue) },
      { case x: WrappedInt => JInt(BigInt(x.value)) }
      ))
  class WrappedIntDistSerializer extends CustomSerializer[WrappedIntDist](format => (
      { case JInt(i)       => WrappedIntDist(i.intValue) },
      { case x: WrappedInt => JInt(BigInt(x.value)) }
      ))

  class IntValueClassDistanceSerializer extends CustomSerializer[IntValueClassDistance](format => (
      {
        case JInt(i) =>
          println("xxxx111")
          IntValueClassDistance(i.intValue)
      },
      {
        case x: IntValueClassDistance =>
          println("xxxx222  AAA")
          JInt(BigInt(x.value))
        case x: Int =>
          println("xxxx222  BBB")
          JInt(BigInt(x))
      }
      ))

  /*


      ser: Formats => (PartialFunction[JValue, A], PartialFunction[Any, JValue])) extends Serializer[A] {

    val Class = implicitly[Manifest[A]].erasure

    def deserialize(implicit format: Formats) = {
      case (TypeInfo(Class, _), json) =>
        if (ser(format)._1.isDefinedAt(json)) ser(format)._1(json)
        else throw new MappingException("Can't convert " + json + " to " + Class)
    }

    def serialize(implicit format: Formats) = ser(format)._2

    *
    */


  test("... CustomSerializer[Root] ...") {
    println("tree4Obj = " + tree4Obj2 + " (not showing non-constructor fields)")

    implicit val formats =
      DefaultFormats +
      new WrappedIntTimeSerializer +
      new WrappedIntDistSerializer +
      new IntValueClassDistanceSerializer
      // +  FieldSerializer[Root](serializer = serializer)

    //implicit val formats = Serialization.formats(NoTypeHints) + new WrappedIntSerializer


    val tree4Json: String = Serialization.write(tree4Obj2)
    //val tree4Json2 = compact(Extraction.decompose(tree4Obj))

    println("tree4Json = " + tree4Json)
    //println("tree4Json2 = " + tree4Json2)
    println()


    // Plain wrapper can be handled with custom class.
    assert(  tree4Json.contains(""""optWrappedTime":11"""))
    assert(! tree4Json.contains(""""optWrappedTime":{"value":11}"""))
    //????assert(  tree4Json.contains(""""wrappedDist":12"""))
    //????assert(! tree4Json.contains(""""wrappedDist":{"value":12}"""))

    // ~Direct use of value class (p: VC) appears as wrapped class.
    //????assert(  tree4Json.contains(""""valueTime":13"""))

    // ~Higher-order use of value class (e.g., p: Some[VC] .... ??
    assert(  tree4Json.contains(""""optValueDist":14"""))
    assert(! tree4Json.contains(""""optValueDist":{"value":14}"""))

    // Plain wrapper can be handled with custom class serializer regardless of
    // where used.
    //??assert(  tree4Json.contains(""""wrappedTime":21"""))


    // @transient is honored.
    //??assert(! tree4Json.contains("""transient"""))


    Serialization.read[Root2](tree4Json)
  }




}
