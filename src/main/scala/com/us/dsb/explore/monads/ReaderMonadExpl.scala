package com.us.dsb.explore.monads

//import scalaz.Reader

class ReaderMonadExpl {

  //type Context  // ?? why no error below (because abstract)?
  type Context = (Int, String)

  case class Reader[A](ctxToA: Context => A) {  // (specific to Context)

    def map[B](aToB: A => B): Reader[B] =
        Reader(aToB compose ctxToA)

    def flatMap[B](aToWrappedBfn: A => Reader[B]): Reader[B] =
        Reader(context => aToWrappedBfn(ctxToA(context)) ctxToA context)
  }


  type ft1 = ((Int, String)) => String
  type ft2 = (String) => Char

  val intStringPairToStringFn: ft1 = pair => pair._2
  val stringToFirstCharFn: ft2 = str => str(0)
  val stringToLastCharFn: ft2 = str => str.last


  // Context-to-specific-pair fn?:
  val r1: Reader[String] = Reader(intStringPairToStringFn)
  val r2a: Reader[Char] = r1.map(stringToFirstCharFn)
  // Context-to-last-char-of-... fn?:
  val r2b: Reader[Char] = r1.map(stringToLastCharFn)
  val r2b2: Reader[Char] = r1.map(_.last)

  val context: (Int, String) = (1, "two")

  //??????:
  val v1 = r1.ctxToA(context)
  val v2a = r2a.ctxToA(context)
  val v2b = r2b.ctxToA(context)


}
