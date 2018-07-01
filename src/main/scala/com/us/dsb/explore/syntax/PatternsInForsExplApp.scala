package com.us.dsb.explore.syntax


/**
  *  Re patterns in for loop/comprehensions.
  */
object PatternsInForsExplApp extends App {

  /*
    Patterns in for comprehensions:
    - For filtering, (generally) needs to be in parentheses:
      - in "for (v: T <- coll)", T must cover element type of coll

      - in "for ((v: T) <- e)",  v gets only the T in coll  ???? NO
  */

  class B
  case class D1(x: Any) extends B
  case class D2(x: Any) extends B

  val c1 = List(new B, D1(1), D1("2"), D2(1))
  val c2 = List((1, 101), (2, "1 0 2"), 3, (4, 404, "FOUR"))


  // 1.  Type at top level does _not_ filter:
  //     - type must cover element type of collection
  //     - contradicts section 8.1.2 of the language spec. (as of 2016-11-13):

  for (v: B <- c1) {           // changing "B" to "D1" yields "type mismatch"
    println("L1: v = " + v)
  }

  // 2.  Type inside tuple or constructor (or extractor?) pattern syntax does
  // filter:

  for ((v1, v2) <- c2) {          // tuple syntax filters
    println("L2.0: v1 = " + v1 + ", v2 = " + v2)
  }
  for ((v1, v2: Int) <- c2) {     // non--top-level type filters (more)
    println("L2.1: v1 = " + v1 + ", v2 = " + v2)
  }
  for (D1(v) <- c1) {             // constructor/extract syntax filters)
    println("L2.2: v = " + v)
  }
  for (D1(v: Int) <- c1) {        // non--top-level type filters (more)
    println("L2.2: v = " + v)
  }

  // 3:  Top-level type can be filtered on by wrapping in pattern binder:

  for (v1 @ (v2: D1) <- c1) {
    println("L3.1: v1 = " + v1 + ", v2 = " + v2)
  }

  // for (v @ (_: D1) <- c1) { ... }  // doesn't work

  // - Workaround extractor object can reduce per-use verbosity:

  object Id { def unapply[T](x: T) = Some(x) }

  for (Id(v: D2) <- c1) {
    println("L3.2: v = " + v)
  }

  // Unsorted:
  // - Can apply extra parentheses (as if Tuple1[...]) (sometimes?):




  /*
    - For loops/comprehensions:

      ...            ::=  ‘for’ (‘(’ Enumerators ‘)’ | ‘{’ Enumerators ‘}’) ...
      Enumerators    ::=  Generator ...
      Generator      ::=  Pattern1 ‘<-’ ...


    - Patterns:

      Pattern         ::=  Pattern1 { ‘|’ Pattern1 }
      Pattern1        ::=  boundvarid ‘:’ TypePat
                        |  ‘_’ ‘:’ TypePat
                        |  Pattern2
      Pattern2        ::=  id [‘@’ Pattern3]
                        |  Pattern3
      Pattern3        ::=  SimplePattern
                        |  SimplePattern {id [nl] SimplePattern}
      SimplePattern   ::=  ‘_’
                        |  varid
                        |  Literal
                        |  StableId
                        |  StableId ‘(’ [Patterns] ‘)’
                        |  StableId ‘(’ [Patterns ‘,’] [id ‘@’] ‘_’ ‘*’ ‘)’
                        |  ‘(’ [Patterns] ‘)’
                        |  XmlPattern
      Patterns        ::=  Pattern {‘,’ Patterns}

      TypePat         ::=  Type

      varid           ::=  lower idrest
      Literal         ::= ...
      Type            ::= ...

      StableId        ::=  id
                        |  Path ‘.’ id
                        |  [id ‘.’] ‘super’ [ClassQualifier] ‘.’ id
      Path            ::=  StableId
                        |  [id ‘.’] this
      ClassQualifier  ::= ‘[’ id ‘]’
  */

}
