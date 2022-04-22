package com.us.dsb.explore.jsonapi.poc1

import com.us.dsb.explore.jsonapi.poc1.Database._
import io.circe.{ACursor, Decoder, Json}

import java.net.URI

object ResponseGeneration {

  //????? clean: change above object to class, change following to class parameter
  // to paramater to
  import EntityMetadata._
  val specificModelViaIntf: EntityMetadata = EntityMetadataImpl
  import specificModelViaIntf._

  //??temporary:
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

           //??? clean "null" to suppressing member:
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

}
