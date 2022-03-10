package com.us.dsb.explore.jsonapisketch.responses

import java.net.URI

object Responses {
  type TBD = Unit
  type Deferred = Unit

  type Arbitrary = Unit   // arbitrary JSON

  type RelatedResourceLink = Deferred

  type Error = Deferred

  type MemberNameSyntax = String  // constrained ...

  type ResourceId = String    // ?? constrained?
  type ResourceType = MemberNameSyntax  // member (field) name syntax
  type FieldName = MemberNameSyntax
  type AttributeName = FieldName
  type RelationshipName = FieldName


  class MetaObject(val others: Arbitrary*)
  class DocMetadata extends MetaObject
  class LinkMetadata extends MetaObject
  class ResourceObjectMetadata extends MetaObject



  trait Link
  case class LinkString(linkURL: URI) extends Link
  case class LinkObject(href: URI, meta: LinkMetadata)  extends Link

  case class LinksObject(self: Option[Link],
                         related: Option[RelatedResourceLink],  //??? Link with RelatedResourceLink?
                         pagination: Option[Deferred],
                         `others?`: Link*)


  // not "id" or "type", nor "links" nor "relationships"
  // ("[foreign] keys SHOULD NOT appear as attributes")
  case class AttributesObject(members: Map[AttributeName, Arbitrary])


  case class RelationshipsObject(tbd: TBD)  //????


  trait PrimaryData

  trait ResourceObjectOrId extends PrimaryData //?? maybe not just primary soon
  trait NonNullResourceObjectOrId extends  ResourceObjectOrId
  case class ResourceObject(id: Some[ResourceId],  // option for case we won't use
                            `type`: TBD,
                            attributes: Option[AttributesObject],
                            relationships: Option[RelationshipsObject],
                            links: Option[Link],
                            meta: Option[ResourceObjectMetadata],
                            `others?`: TBD) extends NonNullResourceObjectOrId
  case class ResourceIdentifierObject(tbd: TBD) extends NonNullResourceObjectOrId
  case class NoResourceObject() extends ResourceObjectOrId

  trait ResourceObjectListOrIdList extends PrimaryData //?? maybe not just primary soon
  case class ResourceObjectList(tbd: List[TBD]) extends ResourceObjectListOrIdList
  case class ResourceIdObjectList(tbd: List[TBD]) extends ResourceObjectListOrIdList


  trait ResponseDoc
  // ignoring: "jsonapi" (implementation description data)


  case class DataResponseDoc(meta: DocMetadata,
                             data: PrimaryData,
                             links: Option[LinksObject],
                             included: Option[List[ResourceObject]],
                             `others?`: TBD*
                             )

  case class ErrorResponseDoc(meta: DocMetadata,
                              errors: List[Error],
                              `links?`: Option[LinksObject], // allowed or not?  meaning what?
                              `others?`: TBD*
                             )
  case class MetaOnlyResponseDoc(meta: DocMetadata,
                                `links?`: Option[LinksObject], // allowed or not?  meaning what?
                                 `others?`: TBD*
                                )

}
