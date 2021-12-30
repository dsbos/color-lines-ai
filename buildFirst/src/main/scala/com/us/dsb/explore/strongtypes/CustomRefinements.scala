package com.us.dsb.explore.strongtypes

/**
 * Separate compilation unit in separate project be reliably compiled first before macro calls are processed (in compilation)
 */
object CustomRefinements {

  import eu.timepit.refined.api.Validate


  /** Predicate that specifies that a string must be a palindrome. */
  case class Palindrome()

  /** Validator for [[Palindrome]] */
  implicit val palindromeValidator: Validate.Plain[String, Palindrome] =
    Validate.fromPredicate(
      v => v == v.reverse,
      p => s"(\"$p\" is not a palindrome)",
      Palindrome())
}
