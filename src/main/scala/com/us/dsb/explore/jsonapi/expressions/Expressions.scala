package com.us.dsb.explore.jsonapi.expressions

object Expressions {


  trait Expression1

  trait Name1 extends Expression1
  case class Ident1(ident: String) extends Name1
  case class CompoundName1(prefix: Name1, nameThis: Ident1)

  trait Literal1 extends Expression1
  case class StringLit1(value: String) extends Literal1
  case class IntLit1(value: Int) extends Literal1
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

  case class FuncCall1(ident: Ident1, args: Expression1*)


  object Temp2 {
    Ident1("function")
    FuncCall1(Ident1("eq"), StringLit1("a"), StringLit1("b"))
    Ident1("attribute")
    FuncCall1(Ident1("startsWith"), StringLit1("prefix"), Ident1("attribute"))
    CompoundName1(Ident1("relationship"), Ident1("name"))
  }

  import com.us.dsb.explore.jsonapi.entitymodel.AttributeInstance
  import com.us.dsb.explore.jsonapi.entitymodel.Entities.UserEntity

  trait FunctionId
  object FunctionId {
    case object Equals extends FunctionId  // polymorphic(?); could do type-specific
    case object Or extends FunctionId
  }
  import FunctionId._

  trait Expression2 {
    //?? type?
  }

  trait Literal2 extends Expression2
  case class StringLit2(value: String) extends Literal2

  case class FuncCall2(f: FunctionId, args: Expression2*) extends Expression2

  case class SimpleAttrRef2(attr: AttributeInstance) extends Expression2

  object Temp3 {
    FuncCall2(Equals, StringLit2("user1"), SimpleAttrRef2(UserEntity.Attributes.name))
    FuncCall2(Or,
              FuncCall2(Equals, StringLit2("user1"), SimpleAttrRef2(UserEntity.Attributes.name)),
              FuncCall2(Equals, StringLit2("user2"), SimpleAttrRef2(UserEntity.Attributes.name)),
              FuncCall2(Equals, StringLit2("user3"), SimpleAttrRef2(UserEntity.Attributes.name)))
  }


}
