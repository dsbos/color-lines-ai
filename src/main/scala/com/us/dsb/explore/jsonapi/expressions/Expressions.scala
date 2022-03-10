package com.us.dsb.explore.jsonapi.expressions

object Expressions {


  trait Expression

  trait Name extends Expression
  case class Ident(ident: String) extends Name
  case class CompoundName(prefix: Name, nameThis: Ident)

  trait Literal extends Expression
  case class StringLit(value: String) extends Literal
  case class IntLit(value: Int) extends Literal
  // what about time values?
  // - use StringLit and promote if context wants time?
  // - maybe t'...'?
  // what about boolean literals?
  // - "true" and "false" identifier syntax (which could theoretically clash with
  //   field (attribute/relationship) names?
  // - "_true" and "_false", which aren't field-name syntax and so can't clash?
  //  (or just assume/require that we won't name any field "true" or "false" (or
  //  require compoung name like ".true"/"data.true"/"user.true" to access))
  // - "'true'" and "'false'" (like possible time literals)?
  // - "b'true'" and "b'false'" (and maybe "t'xxx'" for time literals?)
  //

  case class FuncCall(ident: Ident, args: Expression*)


  object Temp2 {
    Ident("function")
    FuncCall(Ident("eq"), StringLit("a"), StringLit("b"))
    Ident("attribute")
    FuncCall(Ident("startsWith"), StringLit("prefix"), Ident("attribute"))
    CompoundName(Ident("relationship"), Ident("name"))
  }

}
