package com.us.dsb.explore.lang.syntax


/**
  *  Re pattern syntax and patterns in variable declarations.
  */
object PatternSyntaxExplApp extends App {


  /*
     Pattern grammar entry points and references:
     - Pattern:  by CaseClause
     - Pattern1: by Generator (twice) from for loops/comprehensions
     - Pattern2: by PatDef from "val" and "var" variable definitions

     Pattern grammar:

       Pattern         ::=  Pattern1 { ‘|’ Pattern1 }
       Pattern1        ::=  varid ‘:’ TypePat
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
    Type            ::= ....

    StableId        ::=  id
                      |  Path ‘.’ id
                      |  [id ‘.’] ‘super’ [ClassQualifier] ‘.’ id
    Path            ::=  StableId
                      |  [id ‘.’] this
    ClassQualifier  ::= ‘[’ id ‘]’




   */


  /*
    Patterns in value/variable declarations/definitions:

    - Dcl and Def down to PatDef (to Pattern2):

    Dcl          ::=  ‘val’ ValDcl
                   |  ‘var’ VarDcl
                   |  ...
    ValDcl       ::=  ids ‘:’ Type
    VarDcl       ::=  ids ‘:’ Type
    ids          ::=  id {‘,’ id}

    Def          ::=  PatVarDef               -- ('val' or 'var')
                   |  ...

    PatVarDef    ::=  ‘val’ PatDef
                   |  ‘var’ VarDef

    VarDef       ::=  PatDef                  -- parallel to 'val' PatDef
                   |  ids ‘:’ Type ‘=’ ‘_’    -- (default initial value)


    PatDef       ::=  Pattern2 {‘,’ Pattern2} [‘:’ Type] ‘=’ Expr
                      -- "enters" patterns below Pattern and Patternl (whose "or"
                      --   and wildcard/boundvarid make no sense _at_the_top_level_
                      --   for declarations), _but_ ... nested ...


    ...             ::=  PostfixExpr ‘match’ ‘{’ CaseClauses ‘}’
    CaseClauses     ::=  CaseClause {CaseClause}
    CaseClause      ::=  ‘case’ Pattern [Guard] ‘=>’ Block

  */



  case class C(a: Int, b: Int)

  val x1: Int = 0
  val (x2, x3) = (0, 0)
  val (x4: Int, x5: Int) = (0, 0)
  val (x6, x7): (Int, Int) = (0, 0)
  val (x6b: Int, x7b: Int): (Int, Int) = (0, 0)
  val x8 :: x9 = 0 :: Nil
  val C(x10, x11) = C(1, 2)
  val x12 @ C(x13, x14) = C(1, 2)
  val C(x15, x16 @ _) = C(1, 2)

  /*
- Braces/parentheses/patterns/function literals after, e.g,. .map
*/


}
