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
  def makeTopMetadata(primaryType: EntityType,
                      types: EntityType*): Json = {

    /** Returns member name and value. */
    def makeEntityTypeMetadataMember(`type`: EntityType): (String, Json)  = {
      val typeName = getEntityTypeName(`type`)
      val singularLabel = getEntityTypeSingularLabel(`type`)
      val pluralLabel = getEntityTypePluralLabel(`type`)

      val allAttributes = getEntityTypeAttributes(`type`)
      //?? revisit: confirm array elements vs. object members
      // - how inconvenient is "manual" lookup in array (vs. member reference)
      // - ?????
      val attributesValue: Json = {
        Json.fromValues(
          allAttributes.map { attribute =>
            val typeStr = getDataTypeName(getAttributeType(attribute))
            Json.obj(
              "name"    -> Json.fromString(getAttributeName(attribute).raw),
              "uiLabel" -> Json.fromString(getAttributeLabel(attribute).raw),
              "type"    -> Json.fromString(typeStr.raw),
              //??? does visibility move from back end to UI?  should back end
              // reflect columns selection?
              "shown"   -> Json.fromString("TBD") // "visible"? "selected"?
              )
          }
          )
      }
      val entityTypeValue =
        Json.obj(
          "typeName" -> Json.fromString(typeName.raw),
          "uiLabelSingular" -> Json.fromString(singularLabel.raw),
          "uiLabelPlural"   -> Json.fromString(pluralLabel.raw),

          // typeUrlPathSegment
          "attributes" -> attributesValue
          //"relationships"
          )
      typeName.raw -> entityTypeValue
    }

    //?????? make datatype metadata, especially for enumeration type

    val entityTypeMembers = types.map(makeEntityTypeMetadataMember(_))

    //??? split out type metadata from "data metadata" (e.g, counts)

    Json.obj(
      "primaryType" -> Json.fromString(getEntityTypeName(primaryType).raw),
      "entityTypes" -> Json.obj(entityTypeMembers: _*)
      //?????? dataTypes if we need to declare names for enumeration types (having
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

    //??? decide attribute order; does back end return attributes in order for
    // UI columns?  or does UI decide column order and do whatever it needs to
    // implement that (possibly setting "fields", if back end copies order from
    // there)
    val attributesObject = {
      val fields: Iterable[(String, Json)] =
        requestedAttributes.map { attr =>
          val dbColumn = getAttributeColumnName(attr)
          val attrValue = rowColumnNameToValueMap2(dbColumn)  //(later: or SQL expression)
          val attrName = EntityMetadataImpl.getAttributeName(attr)
          attrName.raw -> dbAnyToJson(attrValue)
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
        "type"       -> Json.fromString(typeName.raw),
        "id"         -> dbAnyToJson(entityId),
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
          "meta" -> makeTopMetadata(`type`, `type`, DomainType /*??? temp.: showing multiple */),
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

  //////////////////////////////////////////////////////////////////////

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
    println()
    println("1. Listing users (as if 'GET /someapi/users'):")
    val responseDoc1: Json =
      makeEntityCollectionResponse(URI.create("/someApi"),
                                   UserType,
                                   None,
                                   ())
    println(s"- responseDoc1: $responseDoc1")

    println()
    println("2. Rendering using metadata:")
    //???? demo using UI labels
    // - simulate making HTML with table

    // - label table with entity type's plural UI label
    val primaryTypeName =
      responseDoc1.hcursor.downField("meta").downField("primaryType")
          .as[String].toOption.get
    val primaryTypeJson =
      responseDoc1.hcursor.downField("meta").downField("entityTypes").downField(primaryTypeName)
          .as[Json].toOption.get
    //println("primaryTypeJson = " + primaryTypeJson)
    val tableLabel =
      primaryTypeJson.hcursor.downField("uiLabelPlural")
          .as[Json].toOption.get
    println(s"[HTML]: $tableLabel:")
    println(s"[HTML]: <table>")
    // - make row labeling each column with attribute's UI label
    println(s"[HTML]:   <tr>")
    val attrs = primaryTypeJson.hcursor.downField("attributes")
        .as[List[Json]].toOption.get
    attrs.foreach { attr =>
      //?? currenting defaulting table column order "attributes" odrer
      //???? check whether visible/shown/hidden
      val attrColumnLabel = attr.hcursor.downField("uiLabel").as[String].toOption.get
      println(s"[HTML]:     <td>$attrColumnLabel</td>")

    }
    println(s"[HTML]:   </tr>")

    // - make row per listed entity:
    responseDoc1.hcursor.downField("data")

    val entities = responseDoc1.hcursor.downField("data")
           .as[List[Json]].toOption.get
    entities.foreach { entity =>
      //println("entity = " + entity)
      //?????? FIX: not (row) _sort_ order; field order; does "?fields" determine order?
      //???? Q:  Re sort order:  List attributes metadata in sort order, or
      // specify sort order separately/explicitly?
      // Note:  If we "list" attributes as members instead of array elements,
      // then we can't convey order with them.
      //

      //??? What if attributes aren't elements but are members?  (We

      println(s"[HTML]:   <tr>")

      attrs.foreach { attr =>

        val attrName = attr.hcursor.downField("name").as[String].toOption.get
        val attrTypeName = attr.hcursor.downField("type").as[String].toOption.get
        //?????? add datatype metadata so client can look up names of enumeration
        // types and determine that they are enumeration types)
        //???? maybe soon do physical vs. logical types, so enumeration-type
        // attribute User_SomeEnum can have '<span class="type-enum type-someEnum">'
        // (and then entity and domain name can have "type-string type-entityName");
        // (later, add chains, for "type-string type-entityName type-userName")

        //??? handle absent members (possibly representation of null)
        // - is there reliable difference between null and complete absence of
        //   attribute? probably not in JSON here, but visibility in metadata could
        //   differentiate
        val jsonValue =
          entity.hcursor.downField("attributes").downField(attrName)
              .as[Json].toOption.get
        //println(s"raw: $attrName: $attrType = $jsonValue")
        val renderedHtml = {
          attrTypeName match {
            //??? handle nulls
            case "string" =>
              val typedValue: String = jsonValue.asString.get
              s"""<span class="type-$attrTypeName">$typedValue</span>"""  //?? doesn't encode
            case "int" =>
              val typedValue: Int = jsonValue.as[Int].toOption.get
              s"""<span class="type-$attrTypeName">$typedValue<span>"""  //?? doesn't encode
            //???? temporary hard-coding (hackily-know enumeration type):
            case "someEnum" =>
              val typedValue: String = jsonValue.asString.get
              s"""<span class="type-enum type-$attrTypeName">$typedValue</span>"""  //?? doesn't encode


            case dataType =>
              println(s"UNHANDLED data type: '$dataType'")
              ???
          }
        }
        println(s"""[HTML]:     <td>$renderedHtml</td>""")
      }
      println(s"[HTML]:   </tr>")
    }


    //   - make column cell per enabled attribute:
    //     - per attribute type, read from JSON and render to text/HTML
    //   - (what about JSON:API-level "id" value? maybe <tr id="...">?
    println(s"[HTML]: </table>")

    println()
    println("3. Getting just first user listed (via self link):")

    def getCollectionFirstSelfLinkURL(responseDoc: Json): String = {
      val `hc_/data`: ACursor = responseDoc.hcursor.downField("data")
      val `hc_data[0]` = `hc_/data`.downN(0)  // use as array; get element at offset
      val `hc_data[0].links.self` = `hc_data[0]`.downField("links").downField("self")  // resource object to link value
      //?? also handle link object if we (might) generated the
      val x4: Decoder.Result[String] = `hc_data[0].links.self`.as[String]
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
