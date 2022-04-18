package com.us.dsb.explore.jsonapi.poc1

import com.us.dsb.explore.jsonapi.poc1.Database._
import io.circe.Json

import java.net.URI


object EntityMetadata {
  type TBD = Unit

  trait DataType

  case object DT_String extends DataType
  case object DT_Int extends DataType
  case object DT_Timestamp extends DataType
  case class DT_Enumeration(enumerators: TBD) extends DataType //?? where do specific enumeration types go?
  case class DT_JsonObject1()            extends DataType // (maybe)
  case class DT_JsonObject2(Schema: TBD) extends DataType // (possibly)

  case class DataTypeString(raw: String) extends AnyVal

  //?? address physical type vs. logical type(s)

  sealed trait EntityType
  sealed trait Attribute
  //?? relationships (outgoing half?)

  //?? (refine to member-syntax strings, etc.:)

  case class EntityTypeName(raw: String) extends AnyVal
  case class EntityTypeSingularLabel(raw: String) extends AnyVal
  case class EntityTypePluralLabel(raw: String) extends AnyVal
  case class EntityTypeSegment(raw: String) extends AnyVal

  case class EntityId(raw: String) extends AnyVal

  case class AttributeName(raw: String) extends AnyVal
  case class AttributeLabel(raw: String) extends AnyVal  //(exposed name)

}

import EntityMetadata._

trait EntityMetadata {
  /** (Currently,) not necessarily simple name--e.g., with enumerators */
  def getDataTypeString(`type`: DataType): DataTypeString

  def getEntityTypeName(         `type`: EntityType): EntityTypeName
  def getEntityTypeSingularLabel(`type`: EntityType): EntityTypeSingularLabel
  def getEntityTypePluralLabel(  `type`: EntityType): EntityTypePluralLabel
  def getEntityTypeSegment(      `type`: EntityType): EntityTypeSegment
  def getEntityTypeAttributes(   `type`: EntityType): Seq[Attribute]
  def getEntityTableName(        `type`: EntityType): TableName
  def getEntityTableKeyColumn(   `type`: EntityType): ColumnName

  def getEntityTypeForSegment(segment: EntityTypeSegment): EntityType

  def getAttributeName(attribute: Attribute): AttributeName
  def getAttributeLabel(attribute: Attribute): AttributeLabel
  def getAttributeType(attribute: Attribute): DataType
  def getAttributeColumnName(attribute: Attribute): ColumnName
}

object EntityMetadataImpl extends EntityMetadata {
  override def getDataTypeString(`type`: DataType): DataTypeString = {
    val nameThis =
      `type` match {
        case DT_String => "string"
        case DT_Int => "int"
        //?? what about enumeration types? hacky string?  richer representation?
        // named (and sharable) or anonymous?
      }
    DataTypeString(nameThis)
  }

  // Entity-type identifiers:
  case object UserType   extends EntityType
  case object DomainType extends EntityType

  // Entity-type-attribute identifiers:
  case object User_ObjectGuid   extends Attribute
  case object User_UserName     extends Attribute
  case object User_DomainName   extends Attribute
  case object User_SomeInt      extends Attribute
  case object Domain_ObjectGuid extends Attribute
  case object Domain_DomainName extends Attribute

  // Entity-type--level data:

  private case class EntityTypeData(name: EntityTypeName,
                                    singularLabel: EntityTypeSingularLabel,
                                    pluralLabel: EntityTypePluralLabel,
                                    segment: EntityTypeSegment,
                                    tableName: TableName,
                                    tableKeyColumn: ColumnName,
                                    attributes: Seq[Attribute])

  private def getEntityTypeData(`type`: EntityType): EntityTypeData = {
    //?? rework into map/etc.
    import DatabaseImpl._
    `type` match {
      case UserType =>
        EntityTypeData(EntityTypeName("user"),
                       EntityTypeSingularLabel("User"),
                       EntityTypePluralLabel("Users"),
                       EntityTypeSegment("users"),
                       TableNames.users,
                       UserColumnNames.object_guid,
                       List(User_ObjectGuid,
                            User_UserName,
                            User_DomainName,
                            User_SomeInt))
      case DomainType =>
        EntityTypeData(EntityTypeName("domain"),
                       EntityTypeSingularLabel("Domain"),
                       EntityTypePluralLabel("Domains"),
                       EntityTypeSegment("domains"),
                       TableNames.domains,
                       DomainColumnNames.object_guid,
                       List(Domain_ObjectGuid,
                            Domain_DomainName))
    }
  }

  override def getEntityTypeName(`type`: EntityType): EntityTypeName =
    getEntityTypeData(`type`).name

  override def getEntityTypeSingularLabel(  `type`: EntityType): EntityTypeSingularLabel =
    getEntityTypeData(`type`).singularLabel
  override def getEntityTypePluralLabel(  `type`: EntityType): EntityTypePluralLabel =
    getEntityTypeData(`type`).pluralLabel

  override def getEntityTypeSegment(`type`: EntityType): EntityTypeSegment =
    getEntityTypeData(`type`).segment

  override def getEntityTableName(`type`: EntityType): TableName =
    getEntityTypeData(`type`).tableName

  override def getEntityTableKeyColumn(`type`: EntityType): ColumnName =
    getEntityTypeData(`type`).tableKeyColumn

  override def getEntityTypeAttributes(`type`: EntityType): Seq[Attribute] =
    getEntityTypeData(`type`).attributes


  def getEntityTypeForSegment(segment: EntityTypeSegment): EntityType = {
    segment match {
      case EntityTypeSegment("users") => UserType
    }
  }

  // Attribute--level data:

  private case class AttrData(name: AttributeName,
                              label: AttributeLabel,
                              `type`: DataType,
                              dbColumn: ColumnName)
  private def getAttributeData(attribute: Attribute): AttrData = {
    //?? rework into map/etc.
    import DatabaseImpl._

    val (name: String, label: String, `type`: DataType, colName: ColumnName) =
      attribute match {
        case User_ObjectGuid =>
          ("objectGuid", "GUID",        DT_String, UserColumnNames.object_guid)
        case User_UserName =>
          ("userName",   "Name",        DT_String, UserColumnNames.user_name)
        case User_DomainName =>
          ("domainName", "Domain Name", DT_String, UserColumnNames.domain_name)
        case `User_SomeInt` =>
          ("someInt",    "Some Int",    DT_Int,    UserColumnNames.some_int)

        case Domain_ObjectGuid =>
          ("objectGuid", "GUID",        DT_String, DomainColumnNames.object_guid)
        case Domain_DomainName =>
          ("domainName", "Name",        DT_String, DomainColumnNames.domain_name)

        //?? do something with enumeration
        //?? maybe do some times with same enumeration; how to share?
        // (should "meta" have a "datatypes" member for ~parameterized data-type
        //    classes (e.g., enumeration classes)? should all types be whether,
        //    with simple ones such as "string" being declared as primitive or
        //    build it?)
      }
    AttrData(AttributeName(name), AttributeLabel(label), `type`, colName)
  }

  override def getAttributeName(attribute: Attribute): AttributeName =
    getAttributeData(attribute).name

  override def getAttributeLabel(attribute: Attribute): AttributeLabel =
    getAttributeData(attribute).label

  override def getAttributeType(attribute: Attribute): DataType =
    getAttributeData(attribute).`type`

  override def getAttributeColumnName(attribute: Attribute): ColumnName =
    getAttributeData(attribute).dbColumn

}
