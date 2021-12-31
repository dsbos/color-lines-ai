package com.us.dsb.explore.strongtypes

/**
 * Separate compilation unit in separate project be reliably compiled first
 * before macro calls are processed (in compilation).
 *
 * For RefinedTypesExpl.
 */
object CustomRefinementPredicates {

  import eu.timepit.refined.api.Validate
  
  /** Predicate that specifies that a string must be a palindrome. */
  case class Palindrome()

  /** For testing/probing.  */
  case class CustomClass[T](first: T, last: T)
  

  /** Validator for [[Palindrome]] on String */
  implicit val palindromeValidatorForString: Validate.Plain[String, Palindrome] =
    Validate.fromPredicate(
      v => v == v.reverse,  // unoptimized
      p => s"(\"$p\" is not a palindrome)",
      Palindrome())

  /** Validator for [[Palindrome]] on Int (toString yields palindrome) */
  implicit val palindromeValidatorForInt: Validate.Plain[Int, Palindrome] =
    Validate.fromPredicate(
      v => v.toString == v.toString.reverse,  // unoptimized
      p => s"(\"$p\" is not palindrome (via .toString)",
      Palindrome())

  //??? Why didn't this work for Float before floatPalindromeValidator existed?
  /** Validator for [[Palindrome]] on Number (toString yields palindrome) */
  implicit val palindromeValidatorForNumber: Validate.Plain[Number, Palindrome] =
    Validate.fromPredicate(
      v => v.toString == v.toString.reverse,  // unoptimized
      p => s"(\"$p\" is not a palindrome (via .toString)",
      Palindrome())

  /** Validator for [[Palindrome]] on Float (toString yields palindrome) */
  implicit val palindromeValidatorFor: Validate.Plain[Float, Palindrome] =
    Validate.fromPredicate(
      v => v.toString == v.toString.reverse,  // unoptimized
      p => s"(\"$p\" is not a palindrome (via .toString)",
      Palindrome())

  implicit def palindromeValidatorForCustomClass[T]: Validate.Plain[CustomClass[T], Palindrome] =
    Validate.fromPredicate(
      v => v.first == v.last,
      p => s"(\"$p\" is not a palindrome",
      Palindrome())

  /** Validator for [[Palindrome]] on List[_] */
  implicit def palindromeValidatorForList[E]: Validate.Plain[List[E], Palindrome] =
    Validate.fromPredicate(
      v => v == v.reverse,  // unoptimized
      p => s"(\"$p\" is not a palindrome",
      Palindrome())

}
