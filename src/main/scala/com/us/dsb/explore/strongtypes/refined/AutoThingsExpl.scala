package com.us.dsb.explore.strongtypes.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt

// https://github.com/fthomas/refined?
object AutoThingsExpl extends App {


  object AutoRefineV {
    // Without autoRefineV:
    //  42: PosInt  // ... found: Int(42) required: ...PosInt
    // -13: PosInt  // ... found: Int(-13) required: ...PosInt

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

  object AutoRefineT {
    //import eu.timepit.refined.auto.autoRefineT
  }
  AutoRefineT

  object AutoInfer {
    //import eu.timepit.refined.auto.autoInfer
  }
  AutoInfer

  object AutoUnwrap {
    //import eu.timepit.refined.auto.autoUnwrap
  }
  AutoUnwrap

}
