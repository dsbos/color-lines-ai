package com.us.dsb.explore.lang.syntax


/**
  * Re patterns in for loop/comprehensions.  (Doesn't explore partial functions.)
  */
object PatternsInForsExplApp extends App {

  /*
    Patterns in for comprehensions:
    - For filtering, (generally) needs to be in parentheses:
      - in "for (v: T <- coll)", T must cover element type of coll

      - in "for ((v: T) <- e)",  v gets only the T in coll  ???? NO
  */

  class Base
  case class DerivedOne(x: Any) extends Base
  case class DerivedTwo(x: Any) extends Base

  val coll1: List[Base] = List(new Base,
                               DerivedOne(1),
                               DerivedOne("one"),
                               DerivedTwo(2))
  /** Four items, two Tuple2. */
  val coll2: List[Any] = List((1, 101), (2, "1 0 2"), 3, (4, 404, "FOUR"))


  // 1.  Type at top level does _not_ filter:
  //     - type must cover element type of collection (as of 2.10.4)
  //     - contradicts section 8.1.2 of the language spec. (as of 2016-11-13):

  for (v: Base <- coll1) {
    println("L1: v = " + v)  // Lists all, of course
  }
  // for ((v: Base) <- coll1) ... - same as (v: Base <- coll1) ...

  // Error: (...) type mismatch; found: DerivedOne => Unit; required: Base => ?:
  // for (v: DerivedOne <- coll1) {
  //   println("L1b: v = " + v)
  // }
  // for ((v: DerivedOne) <- coll1) ... - same as for (v: DerivedOne <- coll1) ...
  // (2.10.4 bug?)


  // 2.  Type inside tuple or constructor (or extractor?) pattern syntax does
  // filter:

  for ((v1, v2) <- coll2) {          // tuple syntax filters
    println("L2.0: v1 = " + v1 + ", v2 = " + v2)  // Lists only Tuple2[_, _]
  }
  for ((v1, v2: Int) <- coll2) {     // non--top-level type also filters
    println("L2.1: v1 = " + v1 + ", v2 = " + v2)  // Lists only Tuple2[_, Int]
  }
  for (DerivedOne(v) <- coll1) {     // constructor/extractor syntax filters)
    println("L2.2: v = " + v)        // Lists only DerivedOne with Any
  }
  for (DerivedOne(v: Int) <- coll1) {  // non--top-level type also filters
    println("L2.3: v = " + v)          // Lists only DerivedOne with Int
  }

  // 3:  Top-level type can be filtered on by wrapping in pattern binder:

  for (v1 @ (v2: DerivedOne) <- coll1) {
    println("L3.1: v1 = " + v1 + ", v2 = " + v2)
  }
  // all fail:
  // for (v1 @ (_ : DerivedOne) <- coll1) ...
  // for (v1 @ (    DerivedOne) <- coll1) ...
  // for (_  @ (v2: DerivedOne) <- coll1) ...
  // for (     (v2: DerivedOne) <- coll1) ...
  //}

  // for (v @ (_: D1) <- coll1) { ... }  // doesn't work

  // 3a:  Workaround extractor object can reduce per-use verbosity:

  object Id { def unapply[T](x: T) = Some(x) }

  for (Id(v: DerivedTwo) <- coll1) {
    println("L3.2: v = " + v)           // Lists only DerivedTwo
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
