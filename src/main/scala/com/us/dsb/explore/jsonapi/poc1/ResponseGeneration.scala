package com.us.dsb.explore.jsonapi.poc1

import com.us.dsb.explore.jsonapi.poc1.EntityMetadata._
import com.us.dsb.explore.jsonapi.poc1.Database._

import io.circe.{ACursor, Decoder, Json}

import java.net.URI

//????? clean: evolve into API/service handler/server (take URLs, return responses)
object SpecificModelResponseGeneration extends ResponseGeneration(EntityMetadataImpl)

class ResponseGeneration(model: EntityMetadata) {

  // for exploratory EntityType extension methods (see EntityTypeOps):
  implicit val implicitModel = model

  //??temporary:
  import EntityMetadataImpl.DomainType

  //??? split out type metadata from "data metadata" (e.g, counts)

  // (have _multiple_ entity types only with "included" or multi-target-type
  // relationships:)
  def makeTopMetadata(primaryType: EntityType,
                      entityTypes: EntityType*): Json = {

    /** Returns member name and value. */
    def makeDataTypeMetadataMember(dataType: DataType): (String, Json) = {
      val typeName = model.getDataTypeName(dataType)
      val typeKind = model.getDataTypeKind(dataType)
      val typeKindName = model.getDataKindName(typeKind)

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
      val allAttributes = `type`.attributes
      //?? revisit: confirm array elements vs. object members
      // - how inconvenient is "manual" lookup in array (vs. member reference)
      val attributesValue: Json = {
        Json.fromValues(
          allAttributes.map { attribute =>
            val typeName = model.getDataTypeName(model.getAttributeType(attribute))
            Json.obj(
              "name"    -> Json.fromString(model.getAttributeName(attribute).raw),
              "uiLabel" -> Json.fromString(model.getAttributeLabel(attribute).raw),
              "type"    -> Json.fromString(typeName.raw),
              //??? does visibility move from back end to UI?  should back end
              // reflect columns selection?
              "shown"   -> Json.fromString("TBD") // "visible"? "selected"?
              )
          }
          )
      }
      val typeNameStr = `type`.name.raw
      val entityTypeValue =
        Json.obj(
          "typeName" -> Json.fromString(typeNameStr),
          "pathSegment" -> Json.fromString(`type`.pathSegment.raw),
          "uiLabelSingular" -> Json.fromString(`type`.singularLabel.raw),
          "uiLabelPlural"   -> Json.fromString(`type`.pluralLabel.raw),

          // typeUrlPathSegment
          "attributes" -> attributesValue
          //"relationships"
          )
      typeNameStr -> entityTypeValue
    }

    /** Gets (de-duplicated) list of all data type in given entity types. */
    def getEntityTypeDataTypes(entityTypes: Seq[EntityType]): Seq[DataType] = {
      val dataTypes =
        entityTypes.flatMap { entityType =>
          entityType.attributes.map(attr => model.getAttributeType(attr))
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
      "primaryEntityType" -> Json.fromString(primaryType.name.raw),
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
          val dbColumn = model.getAttributeColumnName(attr)
          val attrValue = rowColumnNameToValueMap2(dbColumn)  //(later: or SQL expression)
          val attrName = EntityMetadataImpl.getAttributeName(attr)
          attrName.raw -> dbAnyToJson(attrValue)
        }
      Json.fromFields(fields)
    }
    val entityId = {
      val dbColumn = `type`.tableKeyColumn
      val value = rowColumnNameToValueMap2(dbColumn)
      value
    }
    val rowResourceObject =
      Json.obj(
        "type"       -> Json.fromString(`type`.name.raw),
        "id"         -> dbAnyToJson(entityId),
        "attributes" -> attributesObject,
        // (no "relationships" yet or in this case)
        "links" -> Json.obj(
          "self" -> Json.fromString(
            s"$apiUrlPathPrefix/${`type`.pathSegment.raw}/$entityId?<plus parameters (e.g., 'fields')?>")
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
          "meta" -> makeTopMetadata(`type`, `type`, DomainType /*?? temp.: showing multiple */),
          "data" -> primaryData
          )
  }

  def determineRequestedAttributes(`type`: EntityType,
                                   attributeSelection: Option[Seq[Attribute]]
                                   ): Seq[Attribute] = {
    // default to all attributes (and no relationships, once they're implemented)
    attributeSelection.getOrElse(`type`.attributes)
  }

  def determineNetRequestedColumns(`type`: EntityType,
                                   requestedAttributes: Seq[Attribute]
                                  ): Seq[ColumnName] = {
    val columnsForRequestAttributes =
      requestedAttributes.map { attribute =>
        model.getAttributeColumnName(attribute)
      }
    val keyCol = `type`.tableKeyColumn
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
    val table = `type`.tableName
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
      s"$apiUrlPathPrefix/${`type`.pathSegment.raw}/${entityId.raw}",
      primaryData)
  }

  def makeEntityCollectionResponse(apiUrlPathPrefix: URI, // (concat., don't resolve)
                                   `type`            : EntityType,
                                   attributeSelection: Option[Seq[Attribute]],
                                   other             : TBD): Json = {
    val requestedAttributes = determineRequestedAttributes(`type`,
                                                           attributeSelection)
    val table = `type`.tableName
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
      s"$apiUrlPathPrefix/${`type`.pathSegment.raw}",
      primaryData)
  }

  //////////////////////////////////////////////////////////////////////

}
