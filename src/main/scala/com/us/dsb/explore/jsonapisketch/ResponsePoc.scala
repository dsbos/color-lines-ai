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
  trait EntityMetadata {
    import EntityMetadata._

    def getEntityTypeName(`type`: EntityType): String
    def getEntityTypeAttributes(`type`: EntityType): Seq[Attribute]
    def getAttributeName(attribute: Attribute): String
  }
  object EntityMetadata {
    trait EntityType
    trait Attribute
  }
  import EntityMetadata._

  object EntityMetadataImpl extends EntityMetadata {
    case object UserType extends EntityType
    case object User_UserName extends Attribute

    override def getEntityTypeName(`type`: EntityType): String = {
      `type` match {
        case UserType => "user"
      }
    }

    override def getEntityTypeAttributes(`type`: EntityType): Seq[Attribute] = {

      `type` match {
        case UserType => List(User_UserName)
        case _ => ???
      }
    }

    def getAttributeName(attribute: Attribute): String = {
      attribute match {
        case User_UserName => "userName"
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
    def makeMetadata: Json = {
      val attrsInfo = getEntityTypeAttributes(`type`)

      val attributesValue: Json = {
        Json.fromValues(
          attrsInfo.map { attribute =>
              Json.obj(
                "name" -> Json.fromString(getAttributeName(attribute))
                //???? continue
              )
          }
        )
      }
      val entityTypeValue =
        Json.obj(
          "typeName" -> Json.fromString(getEntityTypeName(`type`)),
          // typeUrlPathSegment
          "attributes" -> attributesValue
          //"relationships"

        )

      Json.obj(
        "entityType" -> entityTypeValue

      )
    }

    def makeData: Json = {
      ???
    }

    Json.obj(
      "meta" -> makeMetadata/*,
      "data" -> makeData*/
      )


  }


  val responseDoc =
    makeSingleEntityResponse(UserType,
                             EntityID("123"),
                             ())
  println(s"responseDoc = $responseDoc")
}
