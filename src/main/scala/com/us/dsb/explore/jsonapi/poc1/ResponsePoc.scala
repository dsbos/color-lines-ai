package com.us.dsb.explore.jsonapi.poc1

import com.us.dsb.explore.jsonapi.poc1.Database._
import io.circe.{ACursor, Decoder, Json}

import java.net.URI

object ResponsePoc extends App {

  import EntityMetadata._
  val specificModelViaIntf: EntityMetadata = EntityMetadataImpl
  import specificModelViaIntf._
  //????temporarily:
  import EntityMetadataImpl.DomainType

  //??? split out type metadata from "data metadata" (e.g, counts)

  //?? _multiple_ entity types only for "included" or multi-target-type relationships
  def makeTopMetadata(primaryType: EntityType,
                      entityTypes: EntityType*): Json = {

    /** Returns member name and value. */
    def makeDataTypeMetadataMember(dataType: DataType): (String, Json) = {
      val typeName = getDataTypeName(dataType)
      val typeKind = getDataTypeKind(dataType)
      val typeKindName = getDataKindName(typeKind)

      val dataTypeValue =
         Json.obj(
           //?? shorten names?
           "typeName" -> Json.fromString(typeName.raw),
           "typeKind" -> Json.fromString(typeKindName.raw),

           //???? clean "null" to suppressing member:
           "enumerators" -> {
             typeKind match {  //
               case PrimitiveKind => Json.Null
               case EnumerationKind =>
                 //???? CLEAN asInstanceOf: probably change match from type kind to data type
                 val enumType = dataType.asInstanceOf[DT_Enumeration]
                 Json.arr(
                   enumType.enumerators.map(enum => Json.fromString(enum.raw)): _*
                 )
             }
           }
           //??? soon:  logical vs. physical? type chain? subtypes?
           )
      // Q: Will we return logical numbers as JSON numbers or as strings?
      // (Are we guaranteed to avoid Float and Double NaN/Inf.etc values? Any
      // other considerations?)  Might affect whether logically numeric type
      // needs to specify which way it's represented.
      typeName.raw -> dataTypeValue
    }


    /** Returns member name and value. */
    def makeEntityTypeMetadataMember(`type`: EntityType): (String, Json) = {
      val typeName = getEntityTypeName(`type`)
      val pathSegment = getEntityTypeSegment(`type`)
      val singularLabel = getEntityTypeSingularLabel(`type`)
      val pluralLabel = getEntityTypePluralLabel(`type`)

      val allAttributes = getEntityTypeAttributes(`type`)
      //?? revisit: confirm array elements vs. object members
      // - how inconvenient is "manual" lookup in array (vs. member reference)
      val attributesValue: Json = {
        Json.fromValues(
          allAttributes.map { attribute =>
            val typeName = getDataTypeName(getAttributeType(attribute))
            Json.obj(
              "name"    -> Json.fromString(getAttributeName(attribute).raw),
              "uiLabel" -> Json.fromString(getAttributeLabel(attribute).raw),
              "type"    -> Json.fromString(typeName.raw),
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
          "pathSegment" -> Json.fromString(pathSegment.raw),
          "uiLabelSingular" -> Json.fromString(singularLabel.raw),
          "uiLabelPlural"   -> Json.fromString(pluralLabel.raw),

          // typeUrlPathSegment
          "attributes" -> attributesValue
          //"relationships"
          )
      typeName.raw -> entityTypeValue
    }

    /** Gets (de-duplicated) list of all data type in given entity types. */
    def getEntityTypeDataTypes(entityTypes: Seq[EntityType]): Seq[DataType] = {
      val dataTypes =
        entityTypes.flatMap { entityType =>
          getEntityTypeAttributes(entityType).map(attr => getAttributeType(attr))
        }
            .distinct
      //println("dataTypes = " + dataTypes)
      dataTypes
    }

    val dataTypes = getEntityTypeDataTypes(entityTypes)
    val dataTypeMembers = dataTypes.map(dt => makeDataTypeMetadataMember(dt))
    val entityTypeMembers = entityTypes.map(et => makeEntityTypeMetadataMember(et))


    Json.obj(
      //?? revisit name:  this is means "primary-data type" (or "primary-data
      // entity type") but "primaryDataType" sounds like "primary data type";
      // "primaryEntityType" sounds slightly ambiguous; "primaryDataEntityType"
      // would resolve ambiguities, but is "uglily" long
      "primaryEntityType" -> Json.fromString(getEntityTypeName(primaryType).raw),
      "dataTypes" -> Json.obj(dataTypeMembers: _*),
      "entityTypes" -> Json.obj(entityTypeMembers: _*)
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
      //??? pass original URL (instead of re-creating)?
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
      //??? pass original URL (instead of re-creating)?
      s"$apiUrlPathPrefix/${getEntityTypeSegment(`type`).raw}",
      primaryData)
  }

  //////////////////////////////////////////////////////////////////////

  import EntityMetadataImpl.UserType
  import EntityMetadataImpl.User_UserName

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

    //???? clean; maybe see https://stackoverflow.com/questions/46144555/decoding-structured-json-arrays-with-circe-in-scala

    // - label table with entity type's plural UI label
    val primaryTypeName =
      responseDoc1.hcursor.downField("meta").downField("primaryEntityType")
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
      println(s"[HTML]:     <th>$attrColumnLabel</th>")

    }
    println(s"[HTML]:   </tr>")

    // - make row per listed entity:
    responseDoc1.hcursor.downField("data")

    val entities = responseDoc1.hcursor.downField("data")
           .as[List[Json]].toOption.get
    entities.foreach { entity =>
      //println("entity = " + entity)

      //??? Q: Should response data specify/reflect intended UI column order?
      // - Probably yes, since back end already provides UI support like that in
      //   the current ADMon architecture:
      //   - Back end determines UI column order:  back end gives column
      //     metadata in some order and UI follows that order.
      //   - Back end mostly determines table column labels:  back end gives
      //     lower-camel-case names of JSON members (and for columns) and UI
      //     tries to interpret camel case back into words and then capitalizes
      //     them.
      //   - Back end determines non-table attribute labels:  back end uses
      //     JSON member names formatted as labels (title case, space-separated,
      //     punctuated).
      // - If yes, determine how to convey UI order of attributes:
      //   - Via order in which attributes are listed in metadata (assuming still
      //     array elements, and not object members)?
      //   - Via separate metadata for attribute/column order (and maybe
      //     visibility e.g., separate ~static vs. ~dynamic (basic vs.
      //     UI-support) data)?
      //   - (Via explicit ordinality value in attribute metadata?)
      //   - (If separate, revisit whether attributes metadata is array or object.)
      // - If yes:
      //   - Will UI allow changing column order?
      //   - If so, will it just do so itself, or will it want to tell back end
      //     to use a different order (so UI can just piggyback on existing
      //     order-propagation code?

      println(s"[HTML]:   <tr>")

      attrs.foreach { attr =>

        val attrName = attr.hcursor.downField("name").as[String].toOption.get
        val attrTypeName = attr.hcursor.downField("type").as[String].toOption.get
        //???? maybe soon do physical vs. logical types, so enumeration-type
        // attribute User_SomeEnum can have '<span class="type-enum type-someEnum">'
        // (and then entity and domain name can have "type-string type-entityName");
        // (later, add chains, for "type-string type-entityName type-userName")

        //??? handle absent members (possibly representation of null)
        // - is there reliable difference between null and complete absence of
        //   attribute? probably not in JSON here, but visibility in metadata could
        //   differentiate
        val attrJsonValue =
          entity.hcursor.downField("attributes").downField(attrName)
              .as[Json].toOption.get
        //println(s"raw: $attrName: $attrType = $jsonValue")
        val renderedHtml = {
          val attrDataTypeJson =
            responseDoc1.hcursor.downField("meta").downField("dataTypes").downField(attrTypeName)
                .as[Json].toOption.get
          val attrDataTypeKind =
            attrDataTypeJson.hcursor.downField("typeKind").as[String].toOption.get

          val (classes: String, renderedValue: String) =
            attrDataTypeKind match {
              // (note: those don't do HTML encoding)
              case k @ "primitive" =>
                attrTypeName match {
                  case "string" =>
                    val typedValue: String = attrJsonValue.asString.get
                    (s"type-$attrTypeName", typedValue)
                  case "int" =>
                    val typedValue: Int = attrJsonValue.as[Int].toOption.get
                    (s"type-$attrTypeName", typedValue.toString)
                  case dataType =>
                    println(s"UNHANDLED '$k'-kind data type: '$dataType'")
                    ???
                }
              case k @ "enumeration" =>
                val typedValue: String = attrJsonValue.asString.get
                (s"type-enum type-$attrTypeName", typedValue)
              case kind =>
                println(s"UNHANDLED data type kind: '$kind'")
                ???
            }
          s"""<span class="$classes">$renderedValue</span>"""
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

      println("4. Getting just selected field(s):")
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
