package com.us.dsb.explore.jsonapi.entitymodel

import com.us.dsb.explore.jsonapi.entitymodel


trait AttributeTypeNEW


trait PrimitiveType {
  val name: String  // PrimitiveTypeName?
}
object PrimitiveType {
  class /*??*/NameThis1(val name: String)
  case object StringType extends NameThis1("string")
}

trait AttributeType {
  val name: String  // AttributeTypeName?
  //val baseType: PrimitiveType //???? generalize, to allow subtyping
}
object AttributeType {
  class /*??*/NameThis2(val name: String) extends AttributeType
  case object UserName extends NameThis2("userName")

}


trait Attribute_Sharable {
  val name: String  // FieldName?
  val `type`: AttributeType
}
object Attribute_Sharable {
  class /*??*/NameThis3(val name: String, val `type`: AttributeType) extends Attribute_Sharable
  def apply(name: String, `type`: AttributeType): Attribute_Sharable =
    new /*??*/NameThis3(name, `type`)

  // ??? What about String -> EntityName -> UserName?
  case object EntityName extends NameThis3("name", AttributeType.UserName)
}

trait Attribute_Owned {
  val parentEntity: Entity
  val name: String  // FieldName?
  val `type`: AttributeType
}
object Attribute_Owned {
  class /*??*/NameThis3b(val parentEntity: Entity,
                   val name: String,
                   val `type`: AttributeType) extends Attribute_Owned
  def apply(parentEntity: Entity, name: String, `type`: AttributeType): Attribute_Owned =
    new /*??*/NameThis3b(parentEntity, name, `type`)

  //// ??? What about String -> EntityName -> UserName?
  //case object EntityName extends NameThis3b("name", AttributeType.UserName)
}



trait Relationship {
  val name: String
  val otherEntityType: Entity

}

/**
 * ...
 * @param singularIdentifer ...; e.g., adminUser    (JSON:API type)
 * @param pluralIdentifer   ...; e.g., adminUsers   (JSON:API URL segment)
 * @param singularLabel     ...; e.g., Admin. User  (single-thing UI label)
 * @param pluralLabel       ...; e.g., Admin. Users (things-list UI label)
 * @param singularPhrase    ...; e.g., admin. user  (single-thing message)
 * @param pluralPhrase      ...; e.g., admin. users (multiple-things messages)
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
  "".toLowerCase

}

trait Entity {
  val `type`: EntityTypeNameInfo
  val attributes_1: Set[Attribute_Sharable]
  val attributes_2: Set[Attribute_Owned]
  val relationships: Set[Relationship]
}

trait EntityTableColumn
trait EntityTable



object UserEntity extends Entity {
  val `type`: EntityTypeNameInfo =
    EntityTypeNameInfo("adUser", "AD User", "AD user", "...")
  object Attributes {
    val name_1 = Attribute_Sharable.EntityName
    case object name_2 extends entitymodel.Attribute_Owned.NameThis3b(UserEntity, "name", AttributeType.UserName)
  }
  val attributes_1: Set[Attribute_Sharable] =
    Set(Attributes.name_1
    )
  val attributes_2: Set[Attribute_Owned] =
    Set(Attributes.name_2
    )
  val relationships: Set[Relationship] = Set()

}

//object DomainEntity extends Entity


