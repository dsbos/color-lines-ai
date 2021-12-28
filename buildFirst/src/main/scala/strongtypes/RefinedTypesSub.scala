package strongtypes

/**
 * Separate compilation unit in separate project be reliably compiled first before macro calls are processed (in compilation)
 */
object RefinedTypesSub {

  import eu.timepit.refined.api.Validate
  import eu.timepit.refined.refineV
  import eu.timepit.refined.auto.autoRefineV  // lets <value>: <refined type> work

  case class Palindrome()

  implicit val palindromeValidator: Validate.Plain[String, Palindrome] =
    Validate.fromPredicate(
      v => v == v.reverse,
      p => s"(\"$p\" is not a palindrome)",
      Palindrome())
}
