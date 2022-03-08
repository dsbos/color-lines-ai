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
class SharableAttributeInfo(val fieldName: String,   // FieldName? (re JSON:API "attributes" and "relationships")
                            val uiLabel: String,
                            val `type`: AttributeType  // ?? narrowable on owned attribute instances?
                            // what about database mapping (column)--here? separate layer?
                           )
object SharableAttributeInfo {
  case class GenericAttrInfo(override val fieldName: String,
                             override val uiLabel: String,
                             override val `type`: AttributeType
                            ) extends SharableAttributeInfo(fieldName, uiLabel, `type`)
  // ?? Would we want to try with val and GenericAttrInfo?  Current toString
  // doesn't show values.  But then toString is minor factor.  CHECK JSON codecs.
  case object EntityNameAttr extends SharableAttributeInfo("name",
                                                           "Name",
                                                           AttributeType.EntityNameString)
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
            fieldName: String,
            uiLabel: String,
            `type`: AttributeType
           ): AttributeInstance =
    AttrInst(parentEntity, SharableAttributeInfo.GenericAttrInfo(fieldName, uiLabel, `type`))
}

/**
 * ...
 * @param singularIdentifer ...; e.g., adminUser    (JSON:API type)
 * @param pluralIdentifer   ...; e.g., adminUsers   (JSON:API URL segment)
 * @param singularLabel     ...; e.g., Admin. User  (single-thing UI label)
 * @param pluralLabel       ...; e.g., Admin. Users (things-list UI label)
 * @param singularPhrase    ...; e.g., admin. user  (single-thing phrase)
 * @param pluralPhrase      ...; e.g., admin. users (multiple-things phrase)
 * @param other             e.g., description of entity kind, tool-tips, etc.
 */
case class EntityTypeNameInfo(
  singularIdentifer: String,
  pluralIdentifer: String,
  singularLabel: String,
  pluralLabel: String,
  singularPhrase: String,
  pluralPhrase: String,
  other: String
  )
object EntityTypeNameInfo {
  /** Constructs with plurals made by by appending "s". */
  def apply(singularIdentifer: String,
            singularLabel: String,
            singularPhrase: String,
            other: String): EntityTypeNameInfo =
    EntityTypeNameInfo(singularIdentifer,
                       singularIdentifer + "s",
                       singularLabel,
                       singularLabel + "s",
                       singularPhrase,
                       singularPhrase + "s",
                       other)
}

trait Entity {
  val typeNameInfo: EntityTypeNameInfo
  val attributes: Set[AttributeInstance]
  //??val relationships: Set[xxRelationship]
}


object Entities {
  object UserEntity extends Entity {
    self => // (rename for clarity below)

    val typeNameInfo: EntityTypeNameInfo =
      EntityTypeNameInfo("adUser", "AD User", "AD user", "...")

    /** For directly accessible references: */
    object Attributes {
      // 1. Instance of common attribute:
      // ?? re-check using objects:
      val name = AttributeInstance(self, SharableAttributeInfo.EntityNameAttr)  // ????  narrow type?
      // 2. Instance of regular ("non-common") attribute:
      val special = AttributeInstance(self, "special", "Is Special", AttributeType.BooleanType)
    }

    /** For ~generic references/lookups/listing. */
    val attributes: Set[AttributeInstance] =
      Set(Attributes.name,
          Attributes.special
          )

    //val relationships: Set[Relationship] = Set()
  }

  object DomainEntity extends Entity {
    self => // (rename for clarity below)

    val typeNameInfo: EntityTypeNameInfo =
      EntityTypeNameInfo("adDomain", "AD Domain", "AD domain", "...")

    object Attributes {
      val name = AttributeInstance(self, SharableAttributeInfo.EntityNameAttr) // ????  narrow type?
      val whatever = AttributeInstance(self, "whatever", "Whatever", AttributeType.BooleanType)
    }

    /** For ~generic references/lookups/listing. */
    val attributes: Set[AttributeInstance] =
      Set(Attributes.name,
          Attributes.whatever
          )

    //val relationships: Set[xxRelationship] = Set()
  }

  val entities: Seq[Entity] =
    List(UserEntity, DomainEntity)

}


//trait xxEntityTableColumn
//trait xxEntityTable
//
//trait xxRelationship {
//  val name: String
//  val otherEntityType: xxEntity
//
//}


object Temp extends App {
//  import Entities._
//  println("UserEntity = " + UserEntity)
//  println("UserEntity.typeNameInfo = " + UserEntity.typeNameInfo)
//  println("UserEntity.attributes:" + UserEntity.attributes.mkString("\n- ", "\n- ", "\n"))

  println("Entity types:")
  Entities.entities.foreach { entityType =>
    println("entityType = " + entityType)
    println("entityType.typeNameInfo = " + entityType.typeNameInfo)
    println("entityType.attributes:" + entityType.attributes.mkString("\n- ", "\n- ", "\n"))

  }

}
