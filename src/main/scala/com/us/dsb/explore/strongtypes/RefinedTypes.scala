// ?? do some collections, e.g., NonEmpty, Head, Forall, Index


package com.us.dsb.explore.strongtypes

import scala.util.Try


// https://github.com/fthomas/refined?
object RefinedTypes extends App {

  //import eu.timepit.refined._
  //import eu.timepit.refined.refineV
  //import eu.timepit.refined.api._
  import eu.timepit.refined.api.Refined
  //import eu.timepit.refined.api.RefType
  //import eu.timepit.refined.auto._
  //import eu.timepit.refined.boolean._
  //import eu.timepit.refined.char._
  //import eu.timepit.refined.collection._
  //import eu.timepit.refined.collection.NonEmpty
  //import eu.timepit.refined.generic._
  //import eu.timepit.refined.numeric._
  //import eu.timepit.refined.numeric.Positive
  //import eu.timepit.refined.string._
  //import eu.timepit.refined.types.char._
  //import eu.timepit.refined.types.numeric._
  //import eu.timepit.refined.types.numeric.NonNegInt
  //import eu.timepit.refined.types.numeric.PosInt
  //import eu.timepit.refined.types.string._
  //import eu.timepit.refined.types.string.NonEmptyString
  //import eu.timepit.refined.util.string._



  // Note that NonEmptyString === String Refined NonEmpty

  object RuntimeMethods {
    import eu.timepit.refined.refineV
    import eu.timepit.refined.api.RefType
    import eu.timepit.refined.collection.NonEmpty

    val goodPlainString = "<non-empty string>"
    val badPlainString = ""

    // Note "String Refined NonEmpty" vs. just "NonEmpty":
    // (return types: Either[String, Refined[String, NonEmpty]])
    val check1ResultGood = RefType.applyRef[String Refined NonEmpty](goodPlainString)
    val check1ResultBad  = RefType.applyRef[String Refined NonEmpty](badPlainString)
    val check2ResultGood = refineV[NonEmpty](goodPlainString)
    val check2ResultBad  = refineV[NonEmpty](badPlainString)

    println("check1ResultGood = " + check1ResultGood)
    println("check2ResultBad = " + check2ResultBad)
    println("check1ResultGood = " + check1ResultGood)
    println("check2ResultBad = " + check2ResultBad)

    val x1: Refined[String, NonEmpty] = refineV[NonEmpty].unsafeFrom(goodPlainString)
    println("x1 = " + x1)
    val x2 = Try { refineV[NonEmpty].unsafeFrom(badPlainString) } // IllegalArgumentException
    println("x2 = " + x2)

    // re which: see https://kwark.github.io/refined-in-practice/#38
    //
  }
  RuntimeMethods  // initialize to run checks in there

  object AutoWrapping{
    import eu.timepit.refined.numeric.Positive
    import eu.timepit.refined.types.numeric.NonNegInt
    import eu.timepit.refined.types.numeric.PosInt
    import eu.timepit.refined.types.string.NonEmptyString

    //"non-empty": NonEmptyString  // error (without import eu.timepit.refined.auto.autoRefineV)
    //1: Refined[Int, Positive]
    //1: PosInt

    import eu.timepit.refined.auto.autoRefineV

    "non-empty": NonEmptyString
    1: Refined[Int, Positive]
    1: PosInt
    //0: PosInt

    0: NonNegInt
    //"": NonEmptyString
  }

  object AutoOtherTBD {
    //import eu.timepit.refined.auto.autoRefineT
    //import eu.timepit.refined.auto.autoInfer
    //import eu.timepit.refined.auto.autoUnwrap


    // see https://kwark.github.io/refined-in-practice/#35 ff



  }

  object NestedScalarSyntax {
    import eu.timepit.refined.boolean._  // Or, Not, True, AllOf, etc.
    import eu.timepit.refined.string._   // StartsWith, MatchesRegex, Regex, etc.
    import eu.timepit.refined.auto.autoRefineV  // lets <value>: <refined type> work

    "az":String Refined StartsWith["a"]
    "az":String Refined EndsWith["z"]
    "ax":String Refined (StartsWith["a"] Or EndsWith["z"])
    "xz":String Refined Or[StartsWith["a"], EndsWith["z"]]
    "ax":String Refined Or[StartsWith["a"], EndsWith["z"]]
    //"xx":String Refined Or[StartsWith["a"], EndsWith["z"]]
  }

  object NestedListSyntax {
    import eu.timepit.refined.auto.autoRefineV  // lets <value>: <refined type> work
    import eu.timepit.refined.boolean._  // Or, Not, True, AllOf, etc.
    import eu.timepit.refined.numeric.Divisible
    import shapeless.HNil
    import shapeless.::

    true: Boolean Refined AllOf[True :: HNil]
    //true: Boolean Refined AllOf[True :: False :: HNil]

    //1: Int Refined AllOf[Divisible[2] :: Divisible[3] :: HNil]
    //2: Int Refined AllOf[Divisible[2] :: Divisible[3] :: HNil]
    //3: Int Refined AllOf[Divisible[2] :: Divisible[3] :: HNil]
    //4: Int Refined AllOf[Divisible[2] :: Divisible[3] :: HNil]
    //5: Int Refined AllOf[Divisible[2] :: Divisible[3] :: HNil]
    6: Int Refined AllOf[Divisible[2] :: Divisible[3] :: HNil]
    //7: Int Refined AllOf[Divisible[2] :: Divisible[3] :: HNil]

    //1: Int Refined AnyOf[Divisible[2] :: Divisible[3] :: HNil]
    2: Int Refined AnyOf[Divisible[2] :: Divisible[3] :: HNil]
    3: Int Refined AnyOf[Divisible[2] :: Divisible[3] :: HNil]
    4: Int Refined AnyOf[Divisible[2] :: Divisible[3] :: HNil]
    //5: Int Refined AnyOf[Divisible[2] :: Divisible[3] :: HNil]
    6: Int Refined AnyOf[Divisible[2] :: Divisible[3] :: HNil]
    //7: Int Refined AnyOf[Divisible[2] :: Divisible[3] :: HNil]

    //1: Int Refined OneOf[Divisible[2] :: Divisible[3] :: HNil]
    2: Int Refined OneOf[Divisible[2] :: Divisible[3] :: HNil]
    3: Int Refined OneOf[Divisible[2] :: Divisible[3] :: HNil]
    4: Int Refined OneOf[Divisible[2] :: Divisible[3] :: HNil]
    //5: Int Refined OneOf[Divisible[2] :: Divisible[3] :: HNil]
    //6: Int Refined OneOf[Divisible[2] :: Divisible[3] :: HNil]
    //7: Int Refined OneOf[Divisible[2] :: Divisible[3] :: HNil]

  }


  {
    import eu.timepit.refined._
    import eu.timepit.refined.refineV
    import eu.timepit.refined.api._
    import eu.timepit.refined.api.Refined
    import eu.timepit.refined.api.RefType
    import eu.timepit.refined.auto.autoRefineV  // lets <value>: <refined type> work
    import eu.timepit.refined.boolean._
    import eu.timepit.refined.char._
    import eu.timepit.refined.collection._
    import eu.timepit.refined.collection.NonEmpty
    import eu.timepit.refined.generic._
    import eu.timepit.refined.numeric._
    import eu.timepit.refined.numeric.Positive
    import eu.timepit.refined.string._
    import eu.timepit.refined.types.char._
    import eu.timepit.refined.types.numeric._
    import eu.timepit.refined.types.numeric.NonNegInt
    import eu.timepit.refined.types.numeric.PosInt
    import eu.timepit.refined.types.string._
    import eu.timepit.refined.types.string.NonEmptyString
    import eu.timepit.refined.util.string._

    "xxx": String Refined NonEmpty
    //"xxx": String Refined MatchesRegex["abc"]
    "xxx": String Refined MatchesRegex["x*"]
  }

  object SpecialStringTypes {
    import eu.timepit.refined.auto.autoRefineV  // lets <value>: <refined type> work
    import eu.timepit.refined.string._
    import eu.timepit.refined.util.string._

    "xxx": String Refined Regex
    "(a|b)": String Refined Regex
    //"(a|b": String Refined Regex
    regex("(a|b)")
    //regex("(a|b")

    //"(Bogus (?": String Refined Uri
    //uri("(Bogus (?")
    uri("(Bogus%20(?")

    //"(Bogus (?": String Refined Url
    //url("(Bogus (?")
    //url("(Bogus%20(?")
    //url("xyz:(Bogus%20(?")
    url("http:(Bogus%20(?") // who's not detecting error?

    //uuid("")
    //uuid("xyz")
    //uuid("00000000-0000-0000-0000-00000000000")
    uuid(  "00000000-0000-0000-0000-000000000000")

    //xml("")
    //xml("<x>/<y>")
    xml("<x></x>")
  }


  object CustomPredicates {
    import CustomRefinements._
    import eu.timepit.refined.refineV

    val x1 = refineV[Palindrome]("madam")
    println("x1 = " + x1)
    val x2 = refineV[Palindrome]("abc")
    println("x2 = " + x2)

    import eu.timepit.refined.auto.autoRefineV  // lets <value>: <refined type> work

    // ?? make SBT sub-project:
    // RefinedTypesSub must be compiled first to avoid "exception during macro
    // expansion ... ClassNotFoundException "
    // (see https://github.com/fthomas/refined/blob/master/modules/docs/macro_pitfalls.md):
    //
   "madam": String Refined Palindrome

    //"abc": String Refined Palindrome

  }
  CustomPredicates

}
