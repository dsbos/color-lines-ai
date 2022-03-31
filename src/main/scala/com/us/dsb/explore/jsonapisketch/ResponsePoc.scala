package com.us.dsb.explore.jsonapisketch

import io.circe.Json

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
      val some_int  = ColumnName("some_int")
    }

    private val usersTable = {
      import UserColumnNames._
      Map(
        "fakeguid-user-0123" -> Map(
          object_guid -> "fakeguid-user-0123",
          user_name -> "User 123",
          some_int -> 1
        ),
        "fakeguid-user-0456" -> Map(
          object_guid -> "fakeguid-user-0456",
          user_name -> "User 456",
          some_int -> 2
        )
      )
    }

    val tables = Map(usersTableName -> usersTable)

    import Database._
    override def xxselectAllRows(tableName: TableName,
                                 columnNames: ColumnName*
                                 ): Seq[Map[ColumnName, Any]] = {
      println(s"selectAllRows.1: tableName = $tableName, columnNames = ${columnNames}")
      val tableFullRows = tables(tableName)
      val rows  = {
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



  trait EntityMetadata {
    import EntityMetadata._

    def getEntityTypeName(`type`: EntityType): String
    def getEntityTypeAttributes(`type`: EntityType): Seq[Attribute]
    def getEntityTableName(`type`: EntityType): TableName
    def getEntityTableKeyColumn(`type`: EntityType): ColumnName
    def getAttributeName(attribute: Attribute): String
    def getAttributeType(attribute: Attribute): String
    def getAttributeColumnName(attribute: Attribute): ColumnName
  }
  object EntityMetadata {
    sealed trait EntityType
    sealed trait Attribute
    case class EntityId(raw: String) extends AnyVal
  }
  import EntityMetadata._

  object EntityMetadataImpl extends EntityMetadata {
    case object UserType extends EntityType
    case object User_ObjectGuid extends Attribute
    case object User_UserName extends Attribute
    case object User_SomeInt extends Attribute

    override def getEntityTypeName(`type`: EntityType): String = {
      `type` match {
        case UserType => "user"
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

    override def getAttributeName(attribute: Attribute): String = {  //???? value class
      attribute match {
        case User_ObjectGuid => "objectGuid"
        case User_UserName => "userName"
        case User_SomeInt => "someInt"
      }
    }

    override def getAttributeType(attribute: Attribute): String = {  //???? value class
      attribute match {
        case User_ObjectGuid => "string"
        case User_UserName => "string"
        case User_SomeInt => "int"
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
  import EntityMetadata._
  import EntityMetadataImpl._



  def makeEntityCollectionResponse(`type`: EntityType,
                                   id: EntityId,
                                   other: TBD): Json = {
    val typeNameString = getEntityTypeName(`type`)
    val allAttributes = getEntityTypeAttributes(`type`)
    val requestedAttributes = getEntityTypeAttributes(`type`)

    def makeMetadata: Json = {
      val attributesValue: Json = {
        Json.fromValues(
          allAttributes.map { attribute =>
            Json.obj(
              "name" -> Json.fromString(getAttributeName(attribute)),
              "type" -> Json.fromString(getAttributeType(attribute))
              //?? more (logical types; what else?)
            )
          }
        )
      }
      val entityTypeValue =
        Json.obj(
          "typeName" -> Json.fromString(typeNameString),
          // typeUrlPathSegment
          "attributes" -> attributesValue
          //"relationships"

        )

      Json.obj(
        //?? allow for multiple entity types? (here or elsewhere?)
        "entityType" -> entityTypeValue
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
            case i: Int    => Json.fromInt(i)
          }
        }

        val attributesObject = {
          val fields: Iterable[(String, Json)] =
            requestedAttributes.map { attr =>
              val dbColumn = getAttributeColumnName(attr)
              val value = dbColumnNameToValueMap(dbColumn)
              val attrName = EntityMetadataImpl.getAttributeName(attr)
              attrName -> dbAnyToJson(value)
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
            "type" -> Json.fromString(typeNameString),
            "id" -> dbAnyToJson(entityId),  //??????CONTINUE
            "attributes" -> attributesObject,
            // (no "relationships" yet or in this case)
            //???? "links" and self link
          )
        rowResourceObject
      }
      Json.fromValues(resourceObjects)
    }

    Json.obj(
      "meta" -> makeMetadata,
      "data" -> makeData
      //???? links: "self"; then some URL path/query parsing
      //?? links: pagination
      )


  }


  val responseDoc =
    makeEntityCollectionResponse(UserType,
                                 EntityId("123"),
                                 ())
  println(s"ResponsePoc.x: responseDoc = $responseDoc")
}
