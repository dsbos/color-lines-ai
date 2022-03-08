package com.us.dsb.explore.jsonapi.entitymodel

import com.us.dsb.explore.jsonapi.entitymodel


// ?? Maybe -> DataType if not just for entity attribute types.  (But in filter
//   expressions, still closely related to attribute types.)

trait AttributeType {
  val name: String  // AttributeTypeName?
  // maybe JSON representation type, or codecs
  // maybe Scala representation type
}

class PrimitiveType(val name: String) extends AttributeType {
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
  case object StringType  extends PrimitiveType("string")

  case object EntityNameString extends DerivedType("entityName", StringType)
  case object UserNameString   extends DerivedType("userName", EntityNameString)
  // (even "case object AdminUserName extends DerivedType("adminUserName", UserName)" )
  // ?? UUID/GUID, object GUID, user GUID?
}

/** Attribute information shared between multiple (entity-specific) instances. */
// ?? RENAME:
class SharableAttributeInfo(val name: String,   // FieldName? (re JSON:API "attributes" and "relationships")
                            val `type`: AttributeType  // ?? narrowable on owned attribute instances?
                            // what about database mapping (column)--here? separate layer?
                           )
object SharableAttributeInfo {
  case class GenericAttrInfo(override val name: String,
                             override val `type`: AttributeType
                            ) extends SharableAttributeInfo(name, `type`)
  // ?? try with val and GenericAttrInfo--current toStrig doesn't show values
  case object EntityNameAttr extends SharableAttributeInfo("name", AttributeType.EntityNameString)
  // ?? entity GUID, etc.
}


trait AttributeInstance {
  val parentEntity: Entity  //?? "containing"?
  val baseInfo: SharableAttributeInfo   // ??? compose or inherit?
  // ???? narrowed type?
  // ?? any other narrowing information?
  // ?? any other instance-specific informatino (e.g., DB column name, if around here
  }
object AttributeInstance {
  case class AttrInst(parentEntity: Entity,
                      baseInfo: SharableAttributeInfo
                     ) extends AttributeInstance
  def apply(parentEntity: Entity,
            baseInfo: SharableAttributeInfo
           ): AttributeInstance =
    AttrInst(parentEntity, baseInfo)
  def apply(parentEntity: Entity,
            name: String,
            `type`: AttributeType
           ): AttributeInstance =
    AttrInst(parentEntity, SharableAttributeInfo.GenericAttrInfo(name, `type`))
}

trait Entity {
  //??val `type`: xxEntityTypeNameInfo
  val attributes: Set[AttributeInstance]
  //??val relationships: Set[xxRelationship]
}


object UserEntity extends Entity {
  self =>  // (rename for clarity below)

  //val `type`: xxEntityTypeNameInfo =
  //  xxEntityTypeNameInfo("adUser", "AD User", "AD user", "...")

  /** For directly accessible references: */
  object Attributes {
    // 1. Instance of common attribute:
    // ?? re-check using objects:
    val name = AttributeInstance(self, SharableAttributeInfo.EntityNameAttr)
    // 2. Instance of regular ("non-common") attribute:
    val special = AttributeInstance(self, "special", AttributeType.BooleanType)
  }
  /** For ~generic references/lookups/listing. */
  val attributes: Set[AttributeInstance] =
    Set(Attributes.name,
        Attributes.special
        )

  //val relationships: Set[xxRelationship] = Set()
}

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
//
//trait xxEntityTableColumn
//trait xxEntityTable
//
//
//
//
////object xxDomainEntity extends xxEntity
//
//
//trait xxRelationship {
//  val name: String
//  val otherEntityType: xxEntity
//
//}


object Temp extends App {
  println("UserEntity = " + UserEntity)
  println("UserEntity.attributes:" + UserEntity.attributes.mkString("\n- ", "\n- ", "\n"))

}
