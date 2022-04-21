package com.us.dsb.explore.jsonapi.poc1

import java.net.URI
import io.circe.{ACursor, Decoder, HCursor, Json}

object CrudeClient extends App {
  println("***: CrudeClient")

  // Because not yet by just URL and single entry point:

  import EntityMetadata.EntityId
  import EntityMetadata.EntityTypeSegment
  import EntityMetadataImpl.UserType
  import EntityMetadataImpl.User_UserName
  import EntityMetadataImpl.getEntityTypeForSegment
  import ResponsePoc.makeSingleEntityResponse
  import ResponsePoc.makeEntityCollectionResponse

  {//?? clean

    println()
    println("1. Listing users (as if 'GET /someapi/users'):")
    val listResponseDoc1: Json = makeEntityCollectionResponse(URI.create("/someApi"),
                                                              UserType,
                                                              None,
                                                              ())
    println(s"- listResponseDoc1: $listResponseDoc1")

    println()
    println("2. Rendering using metadata:")

    //???? clean; maybe see https://stackoverflow.com/questions/46144555/decoding-structured-json-arrays-with-circe-in-scala

    //?? clean string literals in core, then clean up prototype client
    
    val docMetaCursor = listResponseDoc1.hcursor.downField("meta")

    // - 1. label table with entity type's plural UI label

    val primaryTypeCursor = {
      val typeName = docMetaCursor.get[String]("primaryEntityType").toOption.get
      val typeJson = docMetaCursor.downField("entityTypes").downField(typeName).as[Json].toOption.get
      typeJson.hcursor
    }
    val tableLabel = primaryTypeCursor.downField("uiLabelPlural").as[String].toOption.get
    println(s"[HTML]: $tableLabel:")

    println(s"[HTML]: <table>")

    // - 2. make row labeling each column with attribute's UI label
    println(s"[HTML]:   <tr>")
    val attrsJson = primaryTypeCursor.downField("attributes").values.get

    attrsJson.foreach { attr =>
      //?? currently defaulting table column order to "attributes" order
      //???? check whether visible/shown/hidden (and below in data rows)
      val attrColumnLabel = attr.hcursor.get[String]("uiLabel").toOption.get
      println(s"[HTML]:     <th>$attrColumnLabel</th>")

    }
    println(s"[HTML]:   </tr>")

    // - 3. make row per listed entity:
    val entities = listResponseDoc1.hcursor.downField("data").values.get
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
      //?? maybe add onclick="..." with expression referring to entity's
      //   self-link URL (see code below crude-HTML generation)

      //  3.1 - make column cell per enabled attribute:
      //       - per attribute type, read from JSON and render to text/HTML
      attrsJson.foreach { attr =>
        val attrName = attr.hcursor.get[String]("name").toOption.get
        val attrTypeName = attr.hcursor.get[String]("type").toOption.get
        //???? maybe soon do physical vs. logical types, so entity and domain
        // name can have "type-string type-entityName", or with type chains,
        // "type-string type-entityName type-userName")

        //??? handle absent members (possibly representation of null)
        // - is there reliable difference between null and complete absence of
        //   attribute? probably not in JSON here, but visibility in metadata could
        //   differentiate
        val attrJsonValue =
          entity.hcursor.downField("attributes").downField(attrName).as[Json].toOption.get
        val renderedHtml = {
          val attrDataTypeJson =
            docMetaCursor.downField("dataTypes").downField(attrTypeName).as[Json].toOption.get
          val attrDataTypeKind =
            attrDataTypeJson.hcursor.get[String]("typeKind").toOption.get

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
      val firstItemUrlStr = getCollectionFirstSelfLinkURL(listResponseDoc1)
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
