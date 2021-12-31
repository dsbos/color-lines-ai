package com.us.dsb.explore.strongtypes.refined

import scala.util.Try

object RuntimeMethodsExpl extends App {

  import eu.timepit.refined.api.Refined


  import eu.timepit.refined.api.RefType
  import eu.timepit.refined.collection.NonEmpty
  import eu.timepit.refined.refineV

  val goodPlainString = "<non-empty string>"
  val badPlainString = ""

  // Note "String Refined NonEmpty" vs. just "NonEmpty":
  // (return types: Either[String, Refined[String, NonEmpty]])
  val check1ResultGood = RefType.applyRef[String Refined NonEmpty](goodPlainString)
  val check1ResultBad = RefType.applyRef[String Refined NonEmpty](badPlainString)
  val check2ResultGood = refineV[NonEmpty](goodPlainString)
  val check2ResultBad = refineV[NonEmpty](badPlainString)

  println("RuntimeMethods:")
  println("check1ResultGood = " + check1ResultGood)
  println("check2ResultBad = " + check2ResultBad)
  println("check1ResultGood = " + check1ResultGood)
  println("check2ResultBad = " + check2ResultBad)

  val x1: Refined[String, NonEmpty] = refineV[NonEmpty].unsafeFrom(goodPlainString)
  println("x1 = " + x1)
  val x2 = Try {
    refineV[NonEmpty].unsafeFrom(badPlainString)
  } // IllegalArgumentException
  println("x2 = " + x2)

  // re which: see https://kwark.github.io/refined-in-practice/#38
  //

}
