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

  case class EntityTypeName(raw: String) extends AnyVal
  case class EntityTypeSegment(raw: String) extends AnyVal
  case class EntityId(raw: String) extends AnyVal
  case class AttributeName(raw: String) extends AnyVal

}

import EntityMetadata._

trait EntityMetadata {
  /** (Currently,) not necessarily simple name--e.g., with enumerators */
  def getDataTypeString(`type`: DataType): DataTypeString

  def getEntityTypeName(      `type`: EntityType): EntityTypeName
  def getEntityTypeSegment(   `type`: EntityType): EntityTypeSegment
  def getEntityTypeAttributes(`type`: EntityType): Seq[Attribute]
  def getEntityTableName(     `type`: EntityType): TableName
  def getEntityTableKeyColumn(`type`: EntityType): ColumnName

  def getEntityTypeForSegment(segment: EntityTypeSegment): EntityType

  def getAttributeName(attribute: Attribute): AttributeName
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
  case object UserType extends EntityType

  // Entity-type-attribute identifiers:
  case object User_ObjectGuid extends Attribute
  case object User_UserName   extends Attribute
  case object User_SomeInt    extends Attribute

  // Entity-type--level data:

  private case class EntityTypeData(name: EntityTypeName,
                            segment: EntityTypeSegment,
                            tableName: TableName,
                            tableKeyColumn: ColumnName,
                            attributes: Seq[Attribute])

  private def getEntityTypeData(`type`: EntityType): EntityTypeData = {
    import DatabaseImpl._
    `type` match {
      case UserType =>
        EntityTypeData(EntityTypeName("user"),
                       EntityTypeSegment("users"),
                       TableNames.users,
                       UserColumnNames.object_guid,
                       List(User_ObjectGuid,
                            User_UserName,
                            User_SomeInt))
    }
  }

  override def getEntityTypeName(`type`: EntityType): EntityTypeName = {
    getEntityTypeData(`type`).name
  }

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
                              `type`: DataType,
                              dbColumn: ColumnName)
  //?? rework into map/etc.
  private def getAttributeData(attribute: Attribute): AttrData = {
    import DatabaseImpl._
    attribute match {
      case User_ObjectGuid =>
        AttrData(AttributeName("objectGuid"), DT_String, UserColumnNames.object_guid)
      case User_UserName =>
        AttrData(AttributeName("userName"),   DT_String, UserColumnNames.user_name)
      case User_SomeInt =>
        AttrData(AttributeName("someInt"),    DT_Int,    UserColumnNames.some_int)
      //?? do something with enumeration
      //?? maybe do some times with same enumeration; how to share?
      // (should "meta" have a "datatypes" member for ~parameterized data-type
      //    classes (e.g., enumeration classes)? should all types be whether,
      //    with simple ones such as "string" being declared as primitive or
      //    build it?)
    }
  }

  override def getAttributeName(attribute: Attribute): AttributeName =
    getAttributeData(attribute).name

  override def getAttributeType(attribute: Attribute): DataType =
    getAttributeData(attribute).`type`

  override def getAttributeColumnName(attribute: Attribute): ColumnName =
    getAttributeData(attribute).dbColumn

}
