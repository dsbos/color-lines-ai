package com.us.dsb.explore.strongtypes.refined

import scala.util.Try

// https://github.com/fthomas/refined?
object RefinedTypesExpl extends App {

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

  object Basic {
    import eu.timepit.refined.types.numeric.PosInt
    import eu.timepit.refined.auto.autoRefineV

    type Offset = PosInt
    //0: PosInt  // "Predicate failed: (0 > 0)."
    var offset: Offset = 1: PosInt
    offset = 2: PosInt
    offset = 3
    //offset = 0  // Predicate failed: (0 > 0).
    val offset2 = 1: PosInt
    offset = offset2


    //????PosInt.unsafeFrom(1)


    // Without autoUnwrap:
    //offset: Int      // ... found: Offset; required: Int
    offset.value: Int

    // Without autoUnwrap:
    import eu.timepit.refined.auto.autoUnwrap

    offset: Int
  }
  Basic

  object NestedScalarSyntax {

    import eu.timepit.refined.auto.autoRefineV
    import eu.timepit.refined.boolean._
    import eu.timepit.refined.string._ // lets <value>: <refined type> work

    "az": String Refined StartsWith["a"]
    "az": String Refined EndsWith["z"]
    "ax": String Refined (StartsWith["a"] Or EndsWith["z"])
    "xz": String Refined Or[StartsWith["a"], EndsWith["z"]]
    "ax": String Refined Or[StartsWith["a"], EndsWith["z"]]
    //"xx":String Refined Or[StartsWith["a"], EndsWith["z"]]
  }
  NestedScalarSyntax

  object NestedListSyntax {

    import eu.timepit.refined.auto.autoRefineV
    import eu.timepit.refined.boolean._
    import eu.timepit.refined.numeric.Divisible
    import shapeless.{::, HNil}

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
  NestedListSyntax

  {
    import eu.timepit.refined.api.Refined
    import eu.timepit.refined.auto.autoRefineV
    import eu.timepit.refined.collection.NonEmpty
    import eu.timepit.refined.string._

    "xxx": String Refined NonEmpty
    //"xxx": String Refined MatchesRegex["abc"]
    "xxx": String Refined MatchesRegex["x*"]
  }

  object SpecialStringTypes {

    import eu.timepit.refined.auto.autoRefineV
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
    uuid("00000000-0000-0000-0000-000000000000")

    //xml("")
    //xml("<x>/<y>")
    xml("<x></x>")
  }
  SpecialStringTypes

}
