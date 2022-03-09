package com.us.dsb.explore.jsonapi.entitymodel

import com.us.dsb.explore.jsonapi.entitymodel


// ?? Maybe -> "DataType" if not just for entity attribute types.  (But in filter
//   expressions, still closely related to attribute types.)

sealed trait DataType {
  import DataType._
  val name: String  // AttributeTypeName?
  // maybe JSON representation type, or codecs
  // maybe Scala representation type
  // somewhere: maybe ordered list of all types in chain (e.g., userName,
  //  entityName, string) so UI can handle most-specific one it currently
  //  knows about (perhaps even adding all as element class values, with CSS
  //  to format for whichever ones UI understands)
  val (rootType: DataType, typeChain: Seq[DataType]) =
    this match {
      case p: PrimitiveType => (p,          this +: Nil)
      case d: DerivedType   => (d.baseType, this +: d.baseType.typeChain)
    }

}


object DataType {

  class PrimitiveType(val name: String) extends DataType {
    // anything specific to primitive type? (JSON value type?)
  }

  // ?? Where will enumeration types fit in?

  class DerivedType(val name: String,
                    val baseType: DataType
                   ) extends DataType {
  }

  // Primitive types:
  case object BooleanType extends PrimitiveType("boolean")
  case object StringType  extends PrimitiveType("string")
  case object TimestampType extends PrimitiveType("timestamp")

  // Derived/semantic types (including chained types):
  case object EntityNameString extends DerivedType("entityName", StringType)
  case object UserNameString   extends DerivedType("userName", EntityNameString)

}

/** Attribute information shared between multiple (entity-specific) instances. */
class SharableAttributeInfo(val fieldName: String,   // FieldName? (re JSON:API "attributes" and "relationships")
                            val uiLabel: String,
                            val `type`: DataType  // ?? narrowable on owned attribute instances?
                            // what about database mapping (column)--here? separate layer?
                           )
object SharableAttributeInfo {
  case class GenericAttrInfo(override val fieldName: String,
                             override val uiLabel: String,
                             override val `type`: DataType
                            ) extends SharableAttributeInfo(fieldName, uiLabel, `type`)
  // ?? Would we want to try with val and GenericAttrInfo?  Current toString
  // doesn't show values.  But then toString is minor factor.  CHECK JSON codecs.
  case class BaseEntityNameAttr(override val `type`: DataType = DataType.EntityNameString)
      extends SharableAttributeInfo("name",
                                    "Name",
                                    `type`)
  case object PlainEntityNameAttr extends SharableAttributeInfo("name",
                                                                "Name",
                                                                 DataType.EntityNameString)

  case object CreationDateAttr extends SharableAttributeInfo("creationDate",  // **
                                                           "Created",
                                                             DataType.TimestampType)
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
            `type`: DataType
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


// ???? Q: How to model overall relationship vs. how it shows up in two entity
//   types (or twice in same entity type (maybe like parent/child things, or
//   group members of groups))

trait OneWayCardinality
object OneWayCardinality {
  case object to1    extends OneWayCardinality
  case object to0or1 extends OneWayCardinality
  case object to1ToN extends OneWayCardinality
  case object to0ToN extends OneWayCardinality
}

//????? combine RelationshipHalf and RelationshipEnd? or make RelationshipHalf
// add just what's needed for JSON:API "relationships" list

trait RelationshipHalf {
  // ??? any owning-entity-type member?
  val fieldName: String  // (name for JSON:API; outgoing-arrow relationship name)
  val otherEntityType: Entity
  val cardinality: OneWayCardinality
  val relationship: Relationship  // ???: link to xxx view of relationship?
}

trait Relationship {
  trait RelationshipEnd {
    val directedName: String  // ?? fieldName and iuLabel?
    val entityType: Entity
    val cardinality: OneWayCardinality
    // ?? any (internal) indication of "one" vs. "other"?
  }
  val undirectedName: String
  val oneEnd: RelationshipEnd
  val otherEnd: RelationshipEnd
  // any need for combined (xxx-to-yyyy) cardinality?
  // ?? where would mapping to DB implementation of relationship go?
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
      // ?? re-check using objects:

      // 1a. Instance of common attribute, no refinement/narrowing:
      val creationDate = AttributeInstance(self, SharableAttributeInfo.CreationDateAttr)

      // 1a. Instance of narrowable common attribute, no refinement/narrowing:
      val name = AttributeInstance(self, SharableAttributeInfo.BaseEntityNameAttr(DataType.UserNameString))

      // 2. Instance of regular ("non-common") attribute:
      val special = AttributeInstance(self, "special", "Is Special", DataType.BooleanType)
    }

    /** For ~generic references/lookups/listing. */
    val attributes: Set[AttributeInstance] =
      Set(Attributes.name,
          Attributes.special,
          Attributes.creationDate
          )

    object RelationShips  //???

    //val relationships: Set[Relationship] = Set()
  }

  object DomainEntity extends Entity {
    self => // (rename for clarity below)

    val typeNameInfo: EntityTypeNameInfo =
      EntityTypeNameInfo("adDomain", "AD Domain", "AD domain", "...")

    object Attributes {
      val name = AttributeInstance(self, SharableAttributeInfo.PlainEntityNameAttr) // ????  narrow type?
      val creationDate = AttributeInstance(self, SharableAttributeInfo.CreationDateAttr)
    }

    val attributes: Set[AttributeInstance] =
      Set(Attributes.name,
          Attributes.creationDate
          )

    //val relationships: Set[xxRelationship] = Set()
  }

  val entities: Seq[Entity] =
    List(UserEntity,
         DomainEntity)

  // ?? any desire/need to model entity subclassing to that single relationship
  //   of domain can represent domain's containment of multiple entity kinds
  //   (e.g., users, computers, groups, whatever)?  (probably would make
  //   relationship modeling harder) ...

  object UserInDomains extends Relationship {
    val undirectedName: String = "userInDomain"

    val oneEnd: RelationshipEnd = new RelationshipEnd {
      val directedName: String = "??user's containing domain"
      val cardinality: OneWayCardinality = OneWayCardinality.to1
      val entityType: Entity = DomainEntity
    }

    val otherEnd: RelationshipEnd = new RelationshipEnd {
      val directedName: String = "??domain's contained users"
      val cardinality: OneWayCardinality = OneWayCardinality.to0ToN
      val entityType: Entity = UserEntity
    }

  }

}

//trait xxEntityTableColumn
//trait xxEntityTable
// - not necessarily table; also table-like SELECT subset of columns; also
//   table-like results of JOIN
//
//something mapping entity attributes and relationships to columns of tables+
//  (e.g., for transforming filter expressions referring to attributes into
//  SQL expressions referring columns)


object Temp extends App {

  println("Entity types:")
  Entities.entities.foreach { entityType =>
    println("* entityType = " + entityType)
    println("  .typeNameInfo = " + entityType.typeNameInfo)
    println("  .attributes:")
    entityType.attributes.foreach { attr =>
      println(s"    * (attr = $attr)")
      println(s"      - parentEntity identifier: '${attr.parentEntity.typeNameInfo.singularIdentifer}'")
      println(s"      - fieldName: '${attr.baseInfo.fieldName}'")
      println(s"      - uiLabel:   '${attr.baseInfo.uiLabel}")
      println(s"      - type name: '${attr.baseInfo.`type`}'")
      println(s"      - type .rootType}: '${attr.baseInfo.`type`.rootType}'")
      println(s"      - type .typeChain}: '${attr.baseInfo.`type`.typeChain}'")
    }

  }

}
