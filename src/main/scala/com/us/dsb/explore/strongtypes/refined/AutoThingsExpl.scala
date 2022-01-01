package com.us.dsb.explore.strongtypes.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.NonNegInt

// *** See doc>:
// https://www.mvndoc.com/c/eu.timepit/refined_sjs0.6_2.11/index.html#eu.timepit.refined.auto$
object AutoThingsExpl extends App {

  object NoAutos {
    // 1. Wrapping literals:

    PosInt(42)
    // PosInt(-13) Predicate failed: (-13 > 0).

    PosInt(42): Int Refined Positive
    PosInt(42): PosInt
    // no (Int Refined Positive)(42)

    Refined.unapply[Int, Positive](PosInt(42))   //???



    //??? (non-auto,) non-literal wrapping

    // 2. Unwrapping:
    val posInt: PosInt = PosInt(42)

    posInt.value: Int
    //posInt: Int  // type mismatch; found ...PosInt; required: Int
    //val x1: Int = posInt // type mismatch; found ...PosInt; required: Int

    // 3. Wrapping non-literals:

    val pos = 42
    // PosInt(pos)  // "compile-time refinement only works with literals"


    {
      import eu.timepit.refined.api.RefType.applyRef
      import eu.timepit.refined.refineV

      // 1: RefType's applyRef: takes whole refined type, returns Either:
      val form1ResultGood = applyRef[Int Refined Positive](42)
      val form1ResultBad = applyRef[Int Refined Positive](-420)
      val form1ResultBad2 = applyRef[PosInt](-420) // form with type alias

      // 2: refined's refineV: takes refinement only, returns Either:
      val form2ResultGood = refineV[Positive](42)
      val form2ResultBad = refineV[Positive](-420)
    }

  }
  object PutSomewhere {
    // Not just literals, but (some) locally static expressions:
    PosInt(1 - 0)
    // PosInt(0 - 1)  // PosInt(0 - 1)
    PosInt(1 * 1)
    // PosInt(1 * 0)  // Predicate failed: (0 > 0).
    PosInt(1 / 1)
    //PosInt(1 / -1)  // Predicate failed: (-1 > 0).
    //PosInt(1 / 0)  // compile-time refinement only works with literals

  }


  object AutoRefineV {
    // Without autoRefineV:
    //  42: PosInt  // ... found: Int(42) required: ...PosInt
    // -13: PosInt  // ... found: Int(-13) required: ...PosInt

    PosInt(42)
    //PosInt(-13)

    // With autoRefineV:
    import eu.timepit.refined.auto.autoRefineV

    42: PosInt     // Works
    //-13: PosInt  // error "Predicate failed: (-13 > 0)."

    val two = 2
    //(two + two): PosInt  // "compile-time refinement only works with literals"

    (1 + 2): PosInt  // compile-time evaluation? - works
    (1 - 0): PosInt  // compile-time evaluation? - works
    //(0 - 1): PosInt  // compile-time evaluation? - Predicate failed: (-1 > 0).
    //(1 / 0): PosInt  // "compile-time refinement only works with literals" i

  }

  // see https://kwark.github.io/refined-in-practice/#35 ff

  object AutoUnwrap {
    import eu.timepit.refined.types.numeric.PosInt

    PosInt(1).value: Int

    import eu.timepit.refined.auto.autoRefineV
    val x: PosInt = 1: PosInt

    // Without autoUnwrap:
    //x: Int      // ... found: PosInt; required: Int
    x.value: Int

    // Without autoUnwrap:
    import eu.timepit.refined.auto.autoUnwrap

    x: Int
  }
  AutoUnwrap

  object AutoInfer {
    // autoInfer eenables converting from type to narrower/safisfied type:


    // def m1(positive: PosInt) = positive: NonNegInt  // type mismatch

    import eu.timepit.refined.auto.autoInfer

    def m2(positive: PosInt) = positive: NonNegInt  // now okay

  }
  AutoInfer

  object AutoRefineT {
    import eu.timepit.refined.auto.autoRefineT
  }
  AutoRefineT

}
