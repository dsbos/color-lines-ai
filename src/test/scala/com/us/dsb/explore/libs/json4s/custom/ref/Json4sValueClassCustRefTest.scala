package com.us.dsb.explore.libs.json4s.custom.ref

//import com.fasterxml.jackson.databind.DeserializationConfig
//import org.json4s.jackson.JsonMethods.compact
//import org.json4s.native.JsonMethods.{compact => Native_compact}
//import org.json4s.{CustomSerializer, DefaultFormats, Extraction, FieldSerializer, JValue, NoTypeHints}
import org.json4s.JsonAST.JInt
import org.json4s.jackson.Serialization
import org.json4s.{CustomSerializer, DefaultFormats, FieldSerializer}
import org.scalatest.funsuite.AnyFunSuite
//import org.scalatest.matchers._


//case class PROBINGIntValueClass(value1: Int) extends AnyVal


// Value class wrapping Int representing one thing; has custom handler:
case class TimeIntValueClass(value: Int) extends AnyVal

// Value class wrapping Int representing second thing; has custom handler:
case class DistIntValueClass(value: Int) extends AnyVal

// Value class wrapping Int representing a different thing; has no handler:
case class OtherIntValueClass(value: Int) extends AnyVal


case class Root(val time: TimeIntValueClass,
                val optTimePresent: Option[TimeIntValueClass],
                val optTimeAbsent: Option[TimeIntValueClass],
                val timeList: List[TimeIntValueClass],
                val dist: DistIntValueClass,
                val optDistPresent: Option[DistIntValueClass],
                val optDistAbsent: Option[DistIntValueClass],
                val distList: List[DistIntValueClass],
                val unhandled: OtherIntValueClass,
                val optUnhandledPresent: Option[OtherIntValueClass],
                val optUnhandledAbsent: Option[OtherIntValueClass],
                val unhandledList: List[OtherIntValueClass])


//noinspection CaseClassParam
class Json4sValueClassCustRefTest extends AnyFunSuite {

  val tree4Obj2 = Root(TimeIntValueClass(11),
                       Some(TimeIntValueClass(12)),
                       None,
                       List(TimeIntValueClass(13),
                            TimeIntValueClass(14)),
                       DistIntValueClass(21),
                       Some(DistIntValueClass(22)),
                       None,
                       List(DistIntValueClass(23),
                            DistIntValueClass(24)),
                       OtherIntValueClass(31),
                       Some(OtherIntValueClass(32)),
                       None,
                       List(OtherIntValueClass(33),
                            OtherIntValueClass(34)))
  // RENAME--this deserializes too
  // Genericize:

  class TimeIntValueClassSerializer extends CustomSerializer[TimeIntValueClass](nameThis => (
      // Deserialization partial function:
      {
        case JInt(i) =>
          println("TimeIntValueClassSerializer.deserialization: i = " + i)
          TimeIntValueClass(i.intValue)
      },
      // Serialization partial function:
      {
        case v: TimeIntValueClass =>
          println("TimeIntValueClassSerializer.serialization.TimeIntValueClass: v = " + v)
          JInt(BigInt(v.value))
        case v: Int =>
          println("TimeIntValueClassSerializer.serialization.Int: v = " + v)
          JInt(BigInt(v))
      }
      ))

  class GenericIntValueClassSerializer[T : Manifest](constructor: Int => T,
                                                     extractor: T => Int)
      extends CustomSerializer[T](_ => (
          {
            case JInt(i) => constructor(i.intValue)
          },
          {
            case v: T =>    JInt(BigInt(extractor(v)))  // (when boxed)
            case v: Int =>  JInt(BigInt(v))             // (when not boxed)
          }
      ))


  class DistIntValueClassSerializer extends GenericIntValueClassSerializer[DistIntValueClass](
    DistIntValueClass, _.value)


  test("... CustomSerializer[Root] ...") {
    println("tree4Obj = " + tree4Obj2 + " (not showing non-constructor fields)")

    val formats1 =
      DefaultFormats +
      new TimeIntValueClassSerializer

    val formats2 =
      DefaultFormats ++
      (new DistIntValueClassSerializer ::
       new TimeIntValueClassSerializer ::
       Nil)

    implicit val formats = formats2


    val tree4Json: String = Serialization.write(tree4Obj2)
    //val tree4Json2 = compact(Extraction.decompose(tree4Obj))

    println("tree4Json = " + tree4Json)
    //println("tree4Json2 = " + tree4Json2)
    println()



    // ~Direct use of value class (p: VC):
    assert(  tree4Json.contains(""""time":11"""))
    assert(! tree4Json.contains(""""time":{"""))

    // ~Higher-order use of value class (e.g., p: Some[VC] .... ??
    assert(  tree4Json.contains(""""optTimePresent":12"""))
    assert(! tree4Json.contains(""""optTimePresent":{"""))

    assert(!  tree4Json.contains(""""optTimeAbsent"""))

    assert(  tree4Json.contains(""""timeList":[13,14]"""))
    assert(! tree4Json.contains(""""timeList":[{"""))


    // ~Direct use of value class (p: VC):
    assert(  tree4Json.contains(""""dist":21"""))
    assert(! tree4Json.contains(""""dist":{"""))

    // ~Higher-order use of value class (e.g., p: Some[VC] .... ??
    assert(  tree4Json.contains(""""optDistPresent":22"""))
    assert(! tree4Json.contains(""""optDistPresent":{"""))

    assert(!  tree4Json.contains(""""optDistAbsent"""))

    assert(  tree4Json.contains(""""distList":[23,24]"""))
    assert(! tree4Json.contains(""""vList":[{"""))


    // NOTE:  Apparently because of type erasure, a non-boxed field of a value
    // class that does not have a custom serializer/deserializer can still get
    // serialized using the custom serializer/deserializer for a value class
    // having the same underlying type.  (Boxed fields/elements and
    // deserialization don't seem to be mixed up the same as that, but not
    // double-checked.)
    assert(  tree4Json.contains(""""unhandled":31"""))  // expected ...:{"value":31}
    assert(! tree4Json.contains(""""unhandled":{"""))  // expected not :31

    assert(  tree4Json.contains(""""optUnhandledPresent":{"value":32},"""))
    assert(! tree4Json.contains("""optUnhandledPresent":32"""))

    assert(!  tree4Json.contains(""""optUnhandledAbsent"""))

    assert(  tree4Json.contains(""""unhandledList":[{"value":33},{"value":34}]"""))
    assert(! tree4Json.contains(""""unhandledList":[1"""))



    val tree4Obj2NameThisVersion = Serialization.read[Root](tree4Json)
    assert(tree4Obj2NameThisVersion == tree4Obj2)
    println("tree4Obj2NameThisVersion = " + tree4Obj2NameThisVersion)
  }




}
