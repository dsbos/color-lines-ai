package com.us.dsb.explore.jsonapisketch

import io.circe.Json

object ResponsePoc extends App {
  /*
   - scala data: "data" will be for one entity, not list
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
    def selectAllRows(tableName: TableName, columnNames: ColumnName*): Seq[Map[ColumnName, Any]]
  }
  object DatabaseImpl extends Database {
    val usersTableName = TableName("users_table")

    val data = Map(
      usersTableName -> Map(
        "123" -> Map(  //???? drop that level of ~association (?; also, model ~PK for JSON:API's "id"
          "user_name" -> "User 123",
          "some_int" -> 1
        ),
        "456" -> Map(
          "user_name" -> "User 456",
          "some_int" -> 2
        )
      )
    )


    import Database._
    override def selectAllRows(tableName: TableName,
                               columnNames: ColumnName*
                              ): Seq[Map[ColumnName, Any]] = {
      println(s"selectAllRows.1: tableName = $tableName, columnNames = ${columnNames}")
      val tableFullRows = data(tableName)
      val rows  = {
        tableFullRows.map { case (key, allColumns) =>
          println(s"selectAllRows.2: key = $key, allColumns = ${allColumns}")
          val selectedColumns: Map[ColumnName, Any] =
            columnNames.map { columnName =>
              println("selectAllRows.3:   columnName = " + columnName)
              columnName -> allColumns(columnName.raw)
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
    def getAttributeName(attribute: Attribute): String
    def getAttributeType(attribute: Attribute): String
    def getAttributeColumnName(attribute: Attribute): ColumnName
  }
  object EntityMetadata {
    sealed trait EntityType
    sealed trait Attribute
  }
  import EntityMetadata._

  object EntityMetadataImpl extends EntityMetadata {
    case object UserType extends EntityType
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

    override def getEntityTypeAttributes(`type`: EntityType): Seq[Attribute] = {
      `type` match {
        case UserType => List(User_UserName, User_SomeInt)
        case _ => ???
      }
    }

    override def getAttributeName(attribute: Attribute): String = {  //???? value class
      attribute match {
        case User_UserName => "userName"
        case User_SomeInt => "someInt"
      }
    }

    override def getAttributeType(attribute: Attribute): String = {  //???? value class
      attribute match {
        case User_UserName => "string"
        case User_SomeInt => "int"
      }
    }

    override def getAttributeColumnName(attribute: Attribute): ColumnName = {
      attribute match {
        case User_UserName => ColumnName("user_name")  //??? maybe have column constants
        case User_SomeInt => ColumnName("some_int")  //??? maybe have column constants
      }
    }

  }
  import EntityMetadata._
  import EntityMetadataImpl._

  case class EntityID(raw: String) extends AnyVal
  type ScalarDataResponseDoc


  def makeSingleEntityResponse(`type`: EntityType,
                               id: EntityID,
                               other: TBD): Json = {
    val typeNameString = getEntityTypeName(`type`)
    val attrsInfo = getEntityTypeAttributes(`type`)

    def makeMetadata: Json = {
      val attributesValue: Json = {
        Json.fromValues(
          attrsInfo.map { attribute =>
              Json.obj(
                "name" -> Json.fromString(getAttributeName(attribute)),
                "type" -> Json.fromString(getAttributeType(attribute))
                //???? continue
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
        "entityType" -> entityTypeValue

      )
    }

    def makeData: Json = {
      // - get table name(+) for entity type

      val table = getEntityTableName(`type`)

      // - get column ~query for each entity attribute
      val dbColumns =
        attrsInfo.map { attribute =>
          getAttributeColumnName(attribute)
      }

      // - execute query (into some intermediate data)
      val dbRows = DatabaseImpl.selectAllRows(table, dbColumns: _*)
      println(s"makeData.x1: dbRows = " + dbRows)

      // - construct JSON response data
      val resourceObjects = dbRows.map { dbColumnNameToValueMap =>
        println("makeData.x2: dbColumnNameToValueMap = " + dbColumnNameToValueMap)

        // ?????? CONTINUE: correspondence between logical attribute names vs. DB column names (in results)
        val attributesObject = {
          def dbAnyToJson(any: Any): Json = {
            any match {
              case s: String => Json.fromString(s)
              case i: Int    => Json.fromInt(i)
            }
          }
          val fields: Iterable[(String, Json)] =
            attrsInfo.map { attr =>
              val dbColumn = getAttributeColumnName(attr)
              val value = dbColumnNameToValueMap(dbColumn)
              val attrName = EntityMetadataImpl.getAttributeName(attr)
              attrName -> dbAnyToJson(value)
            }

            dbColumnNameToValueMap.map { case (dbColumnName, value) =>
              println("makeData.x3: : dbColumnName = " + dbColumnName)
              s"??????NIY($dbColumnName)" -> dbAnyToJson(value)


            }
          println("makeData.x4: 3: fields2 = " + fields)

          Json.fromFields(fields)
        }

        val rowResourceObject =
          Json.obj(
            "type" -> Json.fromString(typeNameString),
            "id" -> Json.fromString("???NIY"),  //??????CONTINUE
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
    makeSingleEntityResponse(UserType,
                             EntityID("123"),
                             ())
  println(s"ResponsePoc.x: responseDoc = $responseDoc")
}
