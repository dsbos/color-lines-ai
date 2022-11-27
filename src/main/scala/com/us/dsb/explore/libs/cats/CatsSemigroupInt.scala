package com.us.dsb.explore.libs.cats

object CatsSemigroupInt extends App {

  import cats.Semigroup

  println(Semigroup[Int].combine(3, 3)) // returns 6 (3 + 3; not 9 (3 * 3), 27 () etc.)
}
