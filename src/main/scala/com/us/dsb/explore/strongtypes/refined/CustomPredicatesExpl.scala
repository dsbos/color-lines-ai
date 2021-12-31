package com.us.dsb.explore.strongtypes.refined

import com.us.dsb.explore.strongtypes.CustomRefinementPredicates

// https://github.com/fthomas/refined?
object CustomPredicatesExpl extends App {

  import eu.timepit.refined.api.Refined



  // Note: CustomRefinementPredicate must be compiled first to avoid
  // "exception during macro expansion ... ClassNotFoundException" (see
  // https://github.com/fthomas/refined/blob/master/modules/docs/macro_pitfalls.md),
  // and therefore is in subproject "buildFirst".)
  //
  import CustomRefinementPredicates._
  import eu.timepit.refined.auto.autoRefineV // lets <value>: <refined type> work


  "madam": String Refined Palindrome
  // ?? TODO:  Add "shouldNot typeCheck" test
  //"abc": String Refined Palindrome - bad value

  // ?? TODO:  Add "shouldNot typeCheck/compile" test
  //List[Int](): List[Int] Refined Palindrome - only literals in that form


  121: Int Refined Palindrome
  //42: Int Refined Palindrome

  0f: Float Refined Palindrome
  //1.0f: Float Refined Palindrome ("1.0")
  1.1f: Float Refined Palindrome

  import eu.timepit.refined.refineV

  println("""refineV[Palindrome]("madam") = """ + refineV[Palindrome]("madam"))
  println("""refineV[Palindrome]("abc")   = """ + refineV[Palindrome]("abc"))

  println("""refineV[Palindrome](TempCustomSeq(1, 1)) = """ + refineV[Palindrome](CustomClass(1, 1)))
  println("""refineV[Palindrome](TempCustomSeq(1, 2)) = """ + refineV[Palindrome](CustomClass(1, 2)))

  println("""refineV[Palindrome](List())        = """ + refineV[Palindrome](List[Int]()))
  println("""refineV[Palindrome](List(1))       = """ + refineV[Palindrome](List(1)))
  println("""refineV[Palindrome](List(1, 2))    = """ + refineV[Palindrome](List(1, 2)))
  println("""refineV[Palindrome](List(1, 2, 1)) = """ + refineV[Palindrome](List(1, 2, 1)))
  refineV[Palindrome](List(1, 2))
}
