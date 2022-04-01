package com.us.dsb.explore.jsonapi.poc1

import io.circe.Json

import java.net.URI

object ResponsePoc extends App {
  /*
   - scalar data: "data" will be for one entity, not list
   - "data": type, id, attributes w/some, relationships w/some
   - "meta": entity type name, attributes and types, relationships and maybe types
   - "links": "self", no "related"
   - "included": not initially

   */

  type TBD = Unit


  object Database {
    case class TableName(raw: String) extends AnyVal

    case class ColumnName(raw: String) extends AnyVal
  }

  import Database._

  trait Database {
    def xxselectAllRows(tableName: TableName, columnNames: ColumnName*): Seq[Map[ColumnName, Any]]
  }

  object DatabaseImpl extends Database {
    val usersTableName = TableName("users_table")

    object UserColumnNames {
      val object_guid = ColumnName("object_guid")
      val user_name = ColumnName("user_name")
      val some_int = ColumnName("some_int")
    }

    private val usersTable = {
      import UserColumnNames._
      Map(
        "user0123-fake-guid" -> Map(
          object_guid -> "user0123-fake-guid",
          user_name -> "User 123",
          some_int -> 1
          ),
        "user0456-fake-guid" -> Map(
          object_guid -> "user0456-fake-guid",
          user_name -> "User 456",
          some_int -> 2
          )
        )
    }

    val tables = Map(usersTableName -> usersTable)

    import Database._

    override def xxselectAllRows(tableName  : TableName,
                                 columnNames: ColumnName*
                                ): Seq[Map[ColumnName, Any]] = {
      println(s"selectAllRows.1: tableName = $tableName, columnNames = ${columnNames}")
      val tableFullRows = tables(tableName)
      val rows = {
        tableFullRows.map { case (key, allColumns) =>
          println(s"selectAllRows.2: key = $key, allColumns = ${allColumns}")
          val selectedColumns: Map[ColumnName, Any] =
            columnNames.map { columnName =>
              println("selectAllRows.3:   columnName = " + columnName)
              columnName -> allColumns(columnName)
            }.toMap
          println("selectAllRows.4:   selectedColumns = " + selectedColumns)
          selectedColumns
        }
      }
      rows.toSeq
    }
  }


  object EntityMetadata {
    trait DataType

    case object DT_String extends DataType

    case object DT_Int extends DataType

    case object DT_Timestamp extends DataType

    case class DT_Enumeration(enumerators: TBD) extends DataType //?? where do specific enumeration types go?

    case class DT_JsonObject1() extends DataType // (maybe)

    case class DT_JsonObjec2t(Schema: TBD) extends DataType // (possibly)

    case class DataTypeString(raw: String) extends AnyVal

    sealed trait EntityType

    sealed trait Attribute

    case class EntityTypeName(raw: String) extends AnyVal

    case class EntityTypeSegment(raw: String) extends AnyVal

    case class EntityId(raw: String) extends AnyVal

    case class AttributeName(raw: String) extends AnyVal

  }

  import EntityMetadata._

  trait EntityMetadata {
    /** (Currently,) not necessarily simple name--e.g., with enumerators */
    def getDataTypeString(`type`: DataType): DataTypeString

    def getEntityTypeName(`type`: EntityType): EntityTypeName

    def getEntityTypeSegment(`type`: EntityType): EntityTypeSegment

    def getEntityTypeAttributes(`type`: EntityType): Seq[Attribute]

    def getEntityTableName(`type`: EntityType): TableName

    def getEntityTableKeyColumn(`type`: EntityType): ColumnName

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
          case null => ???
        }
      DataTypeString(nameThis)
    }


    case object UserType extends EntityType

    case object User_ObjectGuid extends Attribute

    case object User_UserName extends Attribute

    case object User_SomeInt extends Attribute

    override def getEntityTypeName(`type`: EntityType): EntityTypeName = {
      `type` match {
        case UserType => EntityTypeName("user")
      }
    }

    override def getEntityTypeSegment(`type`: EntityType): EntityTypeSegment = {
      `type` match {
        case UserType => EntityTypeSegment("users")
      }
    }

    override def getEntityTableName(`type`: EntityType): TableName = {
      `type` match {
        case UserType => DatabaseImpl.usersTableName
      }
    }

    override def getEntityTableKeyColumn(`type`: EntityType): ColumnName = {
      `type` match {
        case UserType => DatabaseImpl.UserColumnNames.object_guid
      }
    }

    override def getEntityTypeAttributes(`type`: EntityType): Seq[Attribute] = {
      `type` match {
        case UserType => List(User_ObjectGuid, User_UserName, User_SomeInt)
        case _ => ???
      }
    }

    override def getAttributeName(attribute: Attribute): AttributeName = {
      val nameThis =
        attribute match {
          case User_ObjectGuid => "objectGuid"
          case User_UserName => "userName"
          case User_SomeInt => "someInt"
        }
      AttributeName(nameThis)
    }

    override def getAttributeType(attribute: Attribute): DataType = {
      attribute match {
        case User_ObjectGuid => DT_String
        case User_UserName => DT_String
        case User_SomeInt => DT_Int
        //?? do something with enumeration
        //?? maybe do some times with same enumeration; how to share?
        // (should "meta" have a "datatypes" member for ~parameterized data-type
        //    classes (e.g., enumeration classes)? should all types be whether,
        //    with simple ones such as "string" being declared as primitive or
        //    build it?)
      }
    }

    override def getAttributeColumnName(attribute: Attribute): ColumnName = {
      attribute match {
        case User_ObjectGuid => DatabaseImpl.UserColumnNames.object_guid
        case User_UserName => DatabaseImpl.UserColumnNames.user_name
        case User_SomeInt => DatabaseImpl.UserColumnNames.some_int
      }
    }

  }

  import EntityMetadataImpl._


  def makeEntityCollectionResponse(apiUrlPathPrefix: URI, // (concat., don't resolve)
                                   `type`          : EntityType,
                                   id              : EntityId,
                                   other           : TBD): Json = {
    val typeName = getEntityTypeName(`type`)
    val allAttributes = getEntityTypeAttributes(`type`)
    val requestedAttributes = getEntityTypeAttributes(`type`)

    def makeMetadata: Json = {
      val attributesValue: Json = {
        Json.fromValues(
          allAttributes.map { attribute =>
            val typeStr = getDataTypeString(getAttributeType(attribute))
            Json.obj(
              "name" -> Json.fromString(getAttributeName(attribute).raw),
              "type" -> Json.fromString(typeStr.raw)
              //?? more (logical types; what else?)
              )
          }
          )
      }
      val entityTypeValue =
        Json.obj(
          "typeName" -> Json.fromString(typeName.raw),
          // typeUrlPathSegment
          "attributes" -> attributesValue
          //"relationships"

          )

      Json.obj(
        "entityTypes" -> Json.obj(typeName.raw -> entityTypeValue)
        //?? dataTypes if we need to declare names for enumeration types (having
        //  enumerators list separate from references to enumeration type)
        //?? non-type metadata, e.g., entity counts

        )
    }

    def makeData: Json = {
      // - get table name(+) for entity type

      val table = getEntityTableName(`type`)

      // - get column ~query for each entity attribute
      //?? add entity ID column (primary key) if not explicitly requested
      val requestedDbColumns =
      requestedAttributes.map { attribute =>
        getAttributeColumnName(attribute)
      }

      // - execute query (into some intermediate data)
      val dbRows = DatabaseImpl.xxselectAllRows(table, requestedDbColumns: _*)
      println(s"makeData.x1: dbRows = " + dbRows)

      // - construct JSON response data
      val resourceObjects = dbRows.map { dbColumnNameToValueMap =>
        println("makeData.x2: dbColumnNameToValueMap = " + dbColumnNameToValueMap)

        def dbAnyToJson(any: Any): Json = {
          any match {
            case s: String => Json.fromString(s)
            case i: Int => Json.fromInt(i)
          }
        }

        val attributesObject = {
          val fields: Iterable[(String, Json)] =
            requestedAttributes.map { attr =>
              val dbColumn = getAttributeColumnName(attr)
              val value = dbColumnNameToValueMap(dbColumn)
              val attrName = EntityMetadataImpl.getAttributeName(attr)
              attrName.raw -> dbAnyToJson(value)
            }
          println("makeData.x4: fields2 = " + fields)

          Json.fromFields(fields)
        }

        val entityId = {
          val dbColumn = getEntityTableKeyColumn(`type`)
          val value = dbColumnNameToValueMap(dbColumn)
          value
        }


        val rowResourceObject =
          Json.obj(
            "type" -> Json.fromString(typeName.raw),
            "id" -> dbAnyToJson(entityId),
            "attributes" -> attributesObject,
            // (no "relationships" yet or in this case)
            "links" -> Json.obj(
              "self" -> Json.fromString(s"$apiUrlPathPrefix/${getEntityTypeSegment(`type`).raw}/$entityId")

              )
            )
        rowResourceObject
      }
      Json.fromValues(resourceObjects)
    }

    Json.obj(
      "links" -> Json.obj(
        //?? later, include relevant query parameters
        "self" -> Json.fromString(s"$apiUrlPathPrefix/${getEntityTypeSegment(`type`).raw}")
        //?? links: pagination
        ),
      "meta" -> makeMetadata,
      "data" -> makeData
      )


  }


  val responseDoc =
    makeEntityCollectionResponse(URI.create("/someApi"),
                                 UserType,
                                 EntityId("123"),
                                 ())
  println(s"ResponsePoc.x: responseDoc = $responseDoc")
}
