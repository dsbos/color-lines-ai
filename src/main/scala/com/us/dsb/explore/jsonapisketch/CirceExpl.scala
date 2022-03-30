package com.us.dsb.explore.jsonapisketch

import io.circe._
import io.circe.Json.JString

object CirceExpl extends App {

  // Q:  What is difference between JsonObject and (private) JObject?

  // io.circe.Json:

  // - creation:
  //   - scalars:
  val jn = Json.Null
  val jb1 = Json.True
  val jb2 = Json.fromBoolean(false)
  val js = Json.fromString("")
  val ji = Json.fromInt(0)
  val jl = Json.fromLong(0L)
  val jf = Json.fromFloat(0.0f)
  //   and -Double, -BigInt, -BigDecimal; -OrNull.

  //   - composites:
  //     - via varargs:
  val ja1 = Json.arr(Json.Null)
  val jo1 = Json.obj("m" -> Json.Null)

  //     - via Iterable
  val ja2 = Json.fromValues(List(Json.Null))
  val jo2 = Json.fromFields(List("m" -> Json.Null))


  val jo3 = Json.fromJsonObject(JsonObject())
  // also   Json.fromJsonNumber

  // - case detection:
  {
    val x1 = jo1.isNull
    val x2 = jo1.isBoolean
    val x3 = jo1.isString
    val x4 = jo1.isNumber // (no int vs. float, etc.)
    val x5 = jo1.isArray
    val x6 = jo1.isObject
    print("")
  }

  ////////////////////////////////////////////////////////////

  jb1.as[Boolean]

  Json.showJson.show(jo1)


  // what do Json.withXxx do?

  jo1.toString()
  jo1.noSpaces
  jo1.spaces2
  jo1.spaces4
  jo1.spaces4SortKeys

  jo1.name // Json subtype name


  // Q: How to create JString, etc., most directly?  (They're private.)
  // - WAIT: what are JsonNumber vs. JNumber?

  // parse returns Json (not JsonObject)
  val x1 = io.circe.parser.parse("""{"m": "value"}""")
  val x2 = x1.toOption.get
  println("x2 = " + x2)
  x2 \\ "m"
  println(s"""x2 \\ "m" = ${x2 \\ "m"}""")

  // Q:  How to match on Json subclass?


  val x4 = JsonObject("m" -> Json.Null)
  val x5 = JsonObject.singleton("m", Json.Null)
  JsonObject.fromMap(Map("m" ->Json.Null))
  JsonObject.fromIterable(List("m" ->Json.Null))
  //JsonObject.fromFoldable()

  //x5.+:(null)

  // cursor stuff ...

  // optics stuff ...

}
