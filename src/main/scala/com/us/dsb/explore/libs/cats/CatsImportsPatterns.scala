package com.us.dsb.explore.libs.cats

object CatsImportsPatterns extends App {

  // Type classes:
  import cats.Eq
  import cats.Monad

  // Instances of type classes for various ~data types ().
  import cats.instances.int._
  import cats.instances.option._
  import cats.instances.list._
  import cats.instances.invariant._

  // Extension methods for various data and type-class types.
  import cats.syntax.option._
  import cats.syntax.monad._

  1.some

  // "all of Cats’ type classes" (but not syntax extensions)
  import cats._

  // ~"all standard type class instances and all syntax"
  import cats.implicits._

}
