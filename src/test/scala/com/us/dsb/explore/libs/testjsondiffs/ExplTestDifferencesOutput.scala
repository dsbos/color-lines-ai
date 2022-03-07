package com.us.dsb.explore.libs.testjsondiffs

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import io.circe.literal._

class ExplTestDifferencesOutput extends AnyFunSpec {
  it("Note:  Flip `enable` to see failure differences") {
    pending
  }

  // TODO:  Find some way to have separate entry point that has these tests fail
  //   and show their failure messages, without having failures of these tests
  //   break regular (full project-level) test run (without need to edit code).
  val enable = false

  if (enable) {
    describe("object differencing") {
      import com.softwaremill.diffx.scalatest.DiffShouldMatcher._

      it("simple case class") {
        case class C(a: String, b: List[Int], x: Int = 1)

        // (Use semi-automatic derivation; avoid slow compilation (multiple macros).)
        import com.softwaremill.diffx.Diff
        implicit val diffForC = Diff.derived[C]

        val a = C("one", List(1, 2, 3, 4), 0)
        val b = C("two", List(1, 2, 4, 5), 0)

        a shouldMatchTo b
      }

    }

    describe("??1") {
      object NameThis {
        val refJson = json"""{"a":0, "b": 11, "c": 21, "d": [1, 2, 3]                     }"""
        val eqvJson = json"""{"a":0,          "c": 21, "d": [1, 2, 3],    "b": 11         }"""
        val newJson = json"""{"a":0, "b": 12,          "d": [   2, 3, 4],         "e": 41}"""
        //println(s"refJson  = " + refJson.spaces2)
        //println(s"eqvJson = " + eqvJson.spaces2)
        //println(s"newJson  = " + newJson.spaces2)
      }
      import NameThis._

      //??? What about straight from Circe Json?

      //??? Check out diffx -- see Patience algorithm   (with ...remembering._)
      // ????? see "diffx-scalatest-should"

      it("?? plain ScalaTest on Circe (no diffs; with whole values; get IDE 'see difference') (????)") {
        eqvJson shouldEqual (refJson)
        newJson shouldEqual (refJson)
      }

      it("?? Json4s diff (just printing)") {
        //println(s"refJson == eqvJson: " + (refJson == eqvJson))
        //println(s"refJson == newJson: " + (refJson == newJson))

        //import org.json4s._
        import org.json4s.jackson.JsonMethods._ // parse, pretty
        val refJson2 = parse(refJson.toString())
        val eqvJson2 = parse(eqvJson.toString())
        val newJson2 = parse(newJson.toString())

        //println(s"refJson2 = " + pretty(refJson2))
        //println(s"eqvJson2 = " + pretty(eqvJson2))
        //println(s"newJson2 = " + pretty(newJson2))
        //println

        println(s"refJson2 == eqvJson2: " + (refJson2 == eqvJson2))
        println(s"refJson2 == newJson2: " + (refJson2 == newJson2))

        val diff1to2 = refJson2.diff(newJson2)
        val diff2to1 = newJson2.diff(refJson2)

        println(s"refJson2 -> newJson2: ")
        println(s"- additions: " + pretty(diff1to2.added))
        println(s"- deletions: " + pretty(diff1to2.deleted))
        println(s"- changes, new values: " + pretty(diff1to2.changed))
        println(s"- changes, old values: " + pretty(diff2to1.changed))

        println("+: shows as JSON subset")
        println("+: shows old and new")
        println("-: shows whole changed array ")
        println("(-: doesn't automatically show compared values)")
        println("(-: doesn't automatically do ScalaTest failure)")
      }

      it("?? scalatest-circe (no old values in diff; as JSON Patch; whole values) (????)") {
        import com.stephenn.scalatest.circe.JsonMatchers._
        newJson.toString should matchJson(refJson.toString())
        println("+: shows only changes")
        println("-: shows as JSON patch")
        println("--: does NOT show removed")
        println("(+: does automatically show compared values)")
        println("(+: doesn't automatically do ScalaTest failure)")
      }

      it("1. scalatest-argonaut (unified-diffs ~JSON; with removed values; no whole values") {
        import com.stephenn.scalatest.argonaut.JsonMatchers._
        newJson.toString should matchJson(refJson.toString())
      }

      it("?? scalatest-json4s (no removed values; as JSON; with whole values)") {
        import com.stephenn.scalatest.json4s.JsonMatchers._
        newJson.toString should matchJson(refJson.toString())
      }

      it("?? scalatest-jsonassert (with removed values; as labeled output; with whole values)") {
        import com.stephenn.scalatest.jsonassert.JsonMatchers._
        newJson.toString should matchJson(refJson.toString())
      }

      //    it ("?? scalatest-jsoniterscala") {
      //      import com.stephenn.scalatest.jsoniterscala.JsonMatchers._
      //      newJson.toString should matchJson(refJson.toString())
      //    }

      it("no: scalatest-playjson (shows diffs as patch objects toString; with whole values)") {
        import com.stephenn.scalatest.playjson.JsonMatchers._
        newJson.toString should matchJson(refJson.toString())
      }


    }
  }

}
