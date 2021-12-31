package com.us.dsb.explore.strongtypes.refined

import scala.util.Try

object RuntimeMethodsExpl extends App {
  import eu.timepit.refined.api.Refined
  import eu.timepit.refined.numeric.Positive
  import eu.timepit.refined.types.numeric.PosInt

  import eu.timepit.refined.api.RefType
  import eu.timepit.refined.api.RefType.applyRef
  import eu.timepit.refined.refineV

  // 1: RefType's applyRef: takes whole refined type, returns Either:
  val form1ResultGood = applyRef[Int Refined Positive](42)
  val form1ResultBad  = applyRef[Int Refined Positive](-420)
  val form1ResultBad2 = applyRef[PosInt](-420)  // form with type alias

  // 2: refined's refineV: takes refinement only, returns Either:
  val form2ResultGood = refineV[Positive](42)
  val form2ResultBad  = refineV[Positive](-420)

  // 3: refineV.unsafeFrom takes refinement only, returns value or throws IllegalArgumentException
  val form3ResultGood = refineV[Positive].unsafeFrom(42)
  val form3WrappedResultBad = Try { refineV[Positive].unsafeFrom(-420)}

  println("form1ResultGood = " + form1ResultGood)
  println("form1ResultBad  = " + form1ResultBad)
  println("form1ResultGood = " + form1ResultGood)
  println("form2ResultBad  = " + form2ResultBad)
  println("form3ResultGood       = " + form3ResultGood)
  println("form3WrappedResultBad = " + form3WrappedResultBad)

  // re which: see https://kwark.github.io/refined-in-practice/#38

  // What are / what about:
  // - RefType.applyRef's sibling(s):
  //   - applyRefM
  // - eu.timepit.refined.refineV's siblings:
  //   - refineT
  //   - refineMV
  //   - refineMT
  // - something's unsafeWrap
  // - something's unwrap
  // - something's unsafeRewrap
  // - something's unsafeApply

}
