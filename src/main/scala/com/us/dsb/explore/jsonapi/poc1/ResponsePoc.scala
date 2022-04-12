package com.us.dsb.explore.jsonapi.poc1

import com.us.dsb.explore.jsonapi.poc1.Database._
import io.circe.{ACursor, Decoder, Json}

import java.net.URI

object ResponsePoc extends App {
  /*
   - scalar data: "data" will be for one entity, not list
   - "data": type, id, attributes w/some, relationships w/some
   - "meta": entity type name, attributes and types, relationships and maybe types
   - "links": "self", no "related"
   - "included": not initially

   */

  import EntityMetadata._
  import EntityMetadataImpl._

  //?? need emit datatypes too; set probably derived from entity types (and
  // entity types are derived from query--primary data's type or types, included
  // data's type(s), maybe related data's type(s) in relationships)
  def makeTopMetadata(types: EntityType*): Json = {

    /** Returns member name and value. */
    def makeEntityTypeMetadataMember(`type`: EntityType): (String, Json)  = {
      val typeName = getEntityTypeName(`type`)
      val allAttributes = getEntityTypeAttributes(`type`)
      val attributesValue: Json = {
        Json.fromValues(
          allAttributes.map { attribute =>
            val typeStr = getDataTypeString(getAttributeType(attribute))
            Json.obj(
              "name" -> Json.fromString(getAttributeName(attribute).raw),
              "type" -> Json.fromString(typeStr.raw)
              //?? expand "type" to object--allow for more type info (phys., log.; class, parameterized)
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
      typeName.raw -> entityTypeValue
    }

    val entityTypeMembers = types.map(makeEntityTypeMetadataMember(_))

    //??? split out type metadata from "data metadata" (e.g, counts)

    Json.obj(
      "entityTypes" -> Json.obj(entityTypeMembers: _*)
      //?? dataTypes if we need to declare names for enumeration types (having
      //  enumerators list separate from references to enumeration type)
      )
  }


  /** Renders database query result row data to resource object.
   */
  def renderRow(apiUrlPathPrefix: URI,
                `type`: EntityType,
                requestedAttributes: Seq[Attribute],
                rowColumnNameToValueMap2: Map[ColumnName, Any]
               ): Json = {

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
          val value = rowColumnNameToValueMap2(dbColumn)
          val attrName = EntityMetadataImpl.getAttributeName(attr)
          attrName.raw -> dbAnyToJson(value)
        }
      Json.fromFields(fields)
    }
    val entityId = {
      val dbColumn = getEntityTableKeyColumn(`type`)
      val value = rowColumnNameToValueMap2(dbColumn)
      value
    }
    val typeName = getEntityTypeName(`type`)
    val rowResourceObject =
      Json.obj(
        "type" -> Json.fromString(typeName.raw),
        "id" -> dbAnyToJson(entityId),
        "attributes" -> attributesObject,
        // (no "relationships" yet or in this case)
        "links" -> Json.obj(
          "self" -> Json.fromString(
            s"$apiUrlPathPrefix/${getEntityTypeSegment(`type`).raw}/$entityId?<plus parameters (e.g., 'fields')?>")
          //?? do we need to propagate any query parameters?
          )
        )
    rowResourceObject
  }

  def assembleToplevelObject(`type`: EntityType,
                             selfUrlStr: String,
                             primaryData: Json): Json = {
    Json.obj(
          "links" -> Json.obj(
            //?? later, include relevant query parameters
            "self" -> Json.fromString(selfUrlStr + "?<plus any parameters>")
            //?? links: pagination
            ),
          "meta" -> makeTopMetadata(`type`, DomainType /*??? temp.: showing multiple */),
          "data" -> primaryData
          )
  }

  def determineRequestedAttributes(`type`: EntityType,
                                  attributeSelection: Option[Seq[Attribute]]
                                  ): Seq[Attribute] = {
    // default to all attributes (and no relationships, once they're implemented)
    attributeSelection.getOrElse(getEntityTypeAttributes(`type`))
  }

  def determineNetRequestedColumns(`type`: EntityType,
                                   requestedAttributes: Seq[Attribute]
                                  ): Seq[ColumnName] = {
    val columnsForRequestAttributes =
      requestedAttributes.map { attribute =>
        getAttributeColumnName(attribute)
      }
    val keyCol = getEntityTableKeyColumn(`type`)
    val netColumns =
      if (columnsForRequestAttributes.contains(keyCol)) {
        columnsForRequestAttributes
      }
      else {
        columnsForRequestAttributes :+ keyCol
      }
    //println(s"netColumns = $netColumns")
    netColumns
  }

  //??? factor out remaining commonality from single- vs. multiple-entity methods

  def makeSingleEntityResponse(apiUrlPathPrefix: URI, // (concat., don't resolve)
                               `type`            : EntityType,
                               attributeSelection: Option[Seq[Attribute]],
                               entityId          : EntityId,
                               other             : TBD): Json = {
    val requestedAttributes = determineRequestedAttributes(`type`,
                                                           attributeSelection)
    val table = getEntityTableName(`type`)
    val requestedDbColumns = determineNetRequestedColumns(`type`,
                                                          requestedAttributes)

    // Execute query (into some intermediate data form):
    val dbRowOpt = DatabaseImpl.selectSpecificRow(table,
                                                  RowKey(entityId.raw),
                                                  requestedDbColumns: _*)
    //println(s"makeData.x1: dbRowOpt = " + dbRowOpt)

    // Construct JSON response entity data:
    val primaryData: Json = {
      dbRowOpt match {
        case None => Json.Null
        case Some(rowColumnNameToValueMap1) =>
          val resourceObject =
            renderRow(apiUrlPathPrefix, `type`, requestedAttributes, rowColumnNameToValueMap1)
          resourceObject
      }
    }

    assembleToplevelObject(
      `type`,
      s"$apiUrlPathPrefix/${getEntityTypeSegment(`type`).raw}/${entityId.raw}",
      primaryData)
  }

  def makeEntityCollectionResponse(apiUrlPathPrefix: URI, // (concat., don't resolve)
                                   `type`            : EntityType,
                                   attributeSelection: Option[Seq[Attribute]],
                                   other             : TBD): Json = {
    val requestedAttributes = determineRequestedAttributes(`type`,
                                                           attributeSelection)
    val table = getEntityTableName(`type`)
    val requestedDbColumns = determineNetRequestedColumns(`type`,
                                                          requestedAttributes)

    // Execute query (into some intermediate data form)
    val dbRows = DatabaseImpl.selectAllRows(table, requestedDbColumns: _*)
    //println(s"makeData.x1: dbRows = " + dbRows)

    // Construct JSON response entity data
    val resourceObjects = dbRows.map { rowColumnNameToValueMap1 =>
      renderRow(apiUrlPathPrefix, `type`, requestedAttributes, rowColumnNameToValueMap1)
    }
    val primaryData: Json = {
      Json.fromValues(resourceObjects)
    }

    assembleToplevelObject(
      `type`,
      s"$apiUrlPathPrefix/${getEntityTypeSegment(`type`).raw}",
      primaryData)
  }


  if (false) {
    val responseDoc =
      makeSingleEntityResponse(URI.create("/someApi"),
                               UserType,
                               None,
                               EntityId("user0123-fake-guid"),
                               ())
    println(s"ResponsePoc.makeSingleEntityResponse: responseDoc = $responseDoc")
  }
  {
    val responseDoc1: Json =
      makeEntityCollectionResponse(URI.create("/someApi"),
                                   UserType,
                                   None,
                                   ())
    println(s"ResponsePoc.makeEntityCollectionResponse: responseDoc1 = $responseDoc1")

    def getCollectionFirstSelfLinkURL(responseDoc: Json): String = {
      val `hc_/data`: ACursor = responseDoc.hcursor.downField("data")
      //println("`hc_data` = " + `hc_/data`)
      val `hc_data[0]` = `hc_/data`.downN(0)  // use as array; get element at offset
      //println("`hc_data[0]` = " + `hc_data[0]`)
      val `hc_data[0].links.self` = `hc_data[0]`.downField("links").downField("self")  // resource object to link value
      //println("`hc_data[0].links.self` = " + `hc_data[0].links.self`)
      //?? handle link string vs. link object(?)
      val x4: Decoder.Result[String] = `hc_data[0].links.self`.as[String]
      //println("x4 = " + x4)
      val `value_data[0].links.self` = x4.toOption.get
      //println("`value_data[0].links.self` = " + `value_data[0].links.self`)
      `value_data[0].links.self`
    }

    object requestData {
      val firstItemUrlStr = getCollectionFirstSelfLinkURL(responseDoc1)
      println("firstItemUrlStr = " + firstItemUrlStr)

      //???? split query out before this relative path:
      val apiRootRelativeUrlStr = firstItemUrlStr.drop("/someApi/".length)
      println("apiRootRelativeUrlStr = " + apiRootRelativeUrlStr)
      val path :: query = apiRootRelativeUrlStr.split("\\?").toList
      println("path = " + path)
      println("query = " + query)
      val entityTypeSegmentStr :: idSegmentStr :: Nil = path.split("/").toList
      println("entityTypeSegmentStr = " + entityTypeSegmentStr)
      println("idSegmentStr = " + idSegmentStr)
      val entityTypeSegment = EntityTypeSegment(entityTypeSegmentStr)
      val entityType = getEntityTypeForSegment(entityTypeSegment)
      val entityId = EntityId(idSegmentStr)
    }


    {
      val responseDoc2 =
        makeSingleEntityResponse(URI.create("/someApi"),
                                 requestData.entityType,
                                 None,
                                 requestData.entityId,
                                 ())
      println(s"ResponsePoc.makeSingleEntityResponse: responseDoc2 = $responseDoc2")
      val responseDoc2b =
        makeSingleEntityResponse(URI.create("/someApi"),
                                 requestData.entityType,
                                 Some(Seq()),
                                 requestData.entityId,
                                 ())
      println(s"ResponsePoc.makeSingleEntityResponse: responseDoc2b = $responseDoc2b")
      val responseDoc2c =
        makeSingleEntityResponse(URI.create("/someApi"),
                                 requestData.entityType,
                                 Some(Seq(User_UserName)),
                                 requestData.entityId,
                                 ())
      println(s"ResponsePoc.makeSingleEntityResponse: responseDoc2c = $responseDoc2c")
    }


    //???? continue: parse extracted URL into makeSingleEntityResponse call

  }
}
