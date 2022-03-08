package com.us.dsb.explore.jsonapi.entitymodel

import com.us.dsb.explore.jsonapi.entitymodel


// ?? Maybe -> DataType if not just for entity attribute types.  (But in filter
//   expressions, still closely related to attribute types.)

trait AttributeType {
  val name: String  // AttributeTypeName?
  // maybe JSON representation type, or codecs
  // maybe Scala representation type
}

class PrimitiveType(val name: String)  extends AttributeType {
  // anything specific to primitive type?
}

// ?? Where will enumeration types fit in?

class DerivedType(val name: String,
                  val baseType: AttributeType
                 ) extends AttributeType {
  val rootType: AttributeType = {
    baseType match {
      case p: PrimitiveType => p
      case d: DerivedType => d.baseType
    }
  }
}

object AttributeType {
  case object BooleanType extends PrimitiveType("boolean")
  case object StringType extends PrimitiveType("string")
  case object EntityName extends DerivedType("entityName", StringType)
  case object UserName   extends DerivedType("userName", EntityName)
  // (even "case object AdminUserName extends DerivedType("adminUserName", UserName)" )
  // ?? UUID/GUID, object GUID, user GUID?
}

/** Attribute information shared between multiple (entity-specific) instances. */
// ?? RENAME:
class SharableAttributeInfo(
                               val name: String,   // FieldName? (re JSON:API "attributes" and "relationships")
                               val `type`: AttributeType  // ?? narrowable on owned attribute instances?
                               // what about database mapping (column)--here? separate layer?
                           ) {

}
object SharableAttributeInfo {
  // ??? What about String -> EntityName -> UserName?
  case object EntityName extends SharableAttributeInfo("name", AttributeType.EntityName)
  // ?? entity GUID, etc.
}

//trait xxAttribute_Owned {
//  val parentEntity: xxEntity
//  val name: String  // FieldName?
//  val `type`: AttributeType
//}
//object xxAttribute_Owned {
//  class /*??*/NameThis3b(val parentEntity: xxEntity,
//                   val name: String,
//                   val `type`: xxAttributeTypeCHOOSE) extends xxAttribute_Owned
//  def apply(parentEntity: xxEntity, name: String, `type`: xxAttributeTypeCHOOSE): xxAttribute_Owned =
//    new /*??*/NameThis3b(parentEntity, name, `type`)
//
//  //// ??? What about String -> EntityName -> UserName?
//  //case object xxEntityName extends NameThis3b("name", AttributeTypeCHOOSE.UserName)
//}
//
//
//
//trait xxRelationship {
//  val name: String
//  val otherEntityType: xxEntity
//
//}
//
///**
// * ...
// * @param singularIdentifer ...; e.g., adminUser    (JSON:API type)
// * @param pluralIdentifer   ...; e.g., adminUsers   (JSON:API URL segment)
// * @param singularLabel     ...; e.g., Admin. User  (single-thing UI label)
// * @param pluralLabel       ...; e.g., Admin. Users (things-list UI label)
// * @param singularPhrase    ...; e.g., admin. user  (single-thing message)
// * @param pluralPhrase      ...; e.g., admin. users (multiple-things messages)
// * @param other             e.g., description of entity kind, tool-tips, etc.
// */
//case class xxEntityTypeNameInfo(
//  singularIdentifer: String,
//  pluralIdentifer: String,
//  singularLabel: String,
//  pluralLabel: String,
//  singularPhrase: String,
//  pluralPhrase: String,
//  other: String
//  )
//object xxEntityTypeNameInfo {
//  /** Constructs with plurals made by by appending "s". */
//  def apply(singularIdentifer: String,
//            singularLabel: String,
//            singularPhrase: String,
//            other: String): xxEntityTypeNameInfo =
//    xxEntityTypeNameInfo(singularIdentifer,
//                       singularIdentifer + "s",
//                       singularLabel,
//                       singularLabel + "s",
//                       singularPhrase,
//                       singularPhrase + "s",
//                       other)
//  "".toLowerCase
//
//}
//
//trait xxEntity {
//  val `type`: xxEntityTypeNameInfo
//  val attributes_1: Set[xxAttribute_Sharable]
//  val attributes_2: Set[xxAttribute_Owned]
//  val relationships: Set[xxRelationship]
//}
//
//trait xxEntityTableColumn
//trait xxEntityTable
//
//
//
//object xxUserEntity extends xxEntity {
//  val `type`: xxEntityTypeNameInfo =
//    xxEntityTypeNameInfo("adUser", "AD User", "AD user", "...")
//  object xxAttributes {
//    val name_1 = xxAttribute_Sharable.xxEntityName
//    case object xxname_2 extends xxAttribute_Owned.NameThis3b(xxUserEntity, "name", xxAttributeTypeCHOOSE.xxUserName)
//  }
//  val attributes_1: Set[xxAttribute_Sharable] =
//    Set(xxAttributes.name_1
//    )
//  val attributes_2: Set[xxAttribute_Owned] =
//    Set(xxAttributes.xxname_2
//    )
//  val relationships: Set[xxRelationship] = Set()
//
//}
//
////object xxDomainEntity extends xxEntity
//
//
