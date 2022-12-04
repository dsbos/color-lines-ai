package com.us.dsb.explore.libs.cats

object MonoidsExpl extends App {

  import cats.kernel.{Monoid, Semigroup}

  println("Monoid.empty[Int] = " + Monoid.empty[Int])
  println("Monoid.empty[Double] = " + Monoid.empty[Double])
  println("Monoid.empty[Double] = " + Monoid.empty[Double])

  val x1 = Semigroup.combine(1, 2)
  val x2 = Monoid.combine(1, 2)

  val x3 = Monoid.combine(List(1), List(2.3))
  println("x3 = " + x3)

  def combineThree[A: Monoid](a: A, b: A, c: A): A = {
    val M = implicitly(Monoid)
    M.combine(M.combine(a, b), c)
  }
  val x4 = combineThree(1, 2, 3)
  println("x4 = " + x4)

  case class MinAndMaxInt(min: Int, max: Int)

  implicit val implicit1: Monoid[MinAndMaxInt] =
    new Monoid[MinAndMaxInt] {
      def empty: MinAndMaxInt =
        MinAndMaxInt(Int.MaxValue, Int.MinValue)
      def combine(a: MinAndMaxInt, b: MinAndMaxInt): MinAndMaxInt =
        MinAndMaxInt(a.min min b.min, a.max max b.max)
    }

  val intMin0max10 = MinAndMaxInt(0, 10)
  val intMin5max15 = MinAndMaxInt(5, 15)


  case class MinAndMax[O: Ordering](min: O, max: O)

  val genMin0max10 = MinAndMax(0, 10)
  val genMin5max15 = MinAndMax(5, 15)

  val strMinAmaxX = MinAndMax("a", "x")
  val strMinBmaxY = MinAndMax("b", "y")

  implicitly(Ordering[Int])

  implicit def implicit2[O: Ordering]: Monoid[MinAndMax[O]] =
    new Monoid[MinAndMax[O]] {
      def combine(a: MinAndMax[O], b: MinAndMax[O]): MinAndMax[O] = {
        val o = implicitly(Ordering[O])
        MinAndMax(
          o.min(a.min, b.min),
          o.max(a.max, b.max))
      }
      def empty: MinAndMax[O] =
        ???
    }


  val x8i = Monoid.combine(intMin0max10, intMin5max15)
  println("x8i = " + x8i)
  val x8g = Monoid.combine(genMin0max10, genMin5max15)
  println("x8g = " + x8g)
  val x8s = Monoid.combine(strMinAmaxX, strMinBmaxY)
  println("x8s = " + x8s)

  import cats.syntax.semigroup._
  intMin0max10.combine(intMin5max15)
  intMin0max10 combine intMin5max15
  intMin0max10 |+| intMin5max15

  val x9 = List(1, 2).combine(List(3, 4))
  println("x9 = " + x9)
}
