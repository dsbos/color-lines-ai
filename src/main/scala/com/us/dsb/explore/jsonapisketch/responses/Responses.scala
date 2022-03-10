package com.us.dsb.explore.jsonapisketch.responses

import java.net.URI

object Responses {
  type TBD = Unit
  type Deferred = Unit

  type Arbitrary = Unit   // arbitrary JSON

  type RelatedResourceLink = Deferred


  type Error = Deferred // https://jsonapi.org/format/#error-objects

  type MemberNameSyntax = String  // constrained ...

  type ResourceId = String    // ?? constrained?
  type ResourceType = MemberNameSyntax  // member (field) name syntax
  type FieldName = MemberNameSyntax
  type AttributeName = FieldName
  type RelationshipName = FieldName

  //???? move some lower-level types to shared area (e.g., names that appear in
  // filter, sort, include, etc. parameters


  class MetaObject(val others: Arbitrary*)
  class DocMetadata extends MetaObject
  class LinkMetadata extends MetaObject
  class ResourceObjectMetadata extends MetaObject
  class ResourceIdentifierObjectMetadata extends MetaObject
  class RelationshipObjectMetadata extends MetaObject




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


  //???? CONTINUE with relationships, resource linkage, etc.
  // ... at least of one links, data, and meta;
  // ???? research:  data _and_ links? semantics?
  case class RelationshipObject(links: Option[LinksObject],  // with at least "self" or "related" //??? CONTINUE
                                 data: TBD,  //?????? "resource linkage"
                                 meta: RelationshipObjectMetadata
                                )  //????

  case class RelationshipsObject(members: Map[RelationshipName, RelationshipObject])

  trait PrimaryData

  trait ResourceObjectOrId extends PrimaryData //?? maybe not just primary soon
  trait NonNullResourceObjectOrId extends ResourceObjectOrId
  case class ResourceObject(id: Some[ResourceId],  // optional for case we won't use
                            `type`: TBD,
                            attributes: Option[AttributesObject],
                            relationships: Option[RelationshipsObject],
                            links: Option[Link],
                            meta: Option[ResourceObjectMetadata],
                            `others?`: TBD) extends NonNullResourceObjectOrId

  //???: Q:  Does this always identify a resource object in member included _and_
  //   having a "self" URL?  Or does client sometimes need to assemble URL string
  //   from entity type (re "user" for type and "GET /users/123")?
  //   - If primary data is resource _identifier_ object(s), is there alwways
  //     resource object(s) in response document?
  //???: Q: What determines whether primary data is resource objects or resource
  // identifier objects?  Undefined by JSON:API spec.?  Typically fixed per
  // service?  Typically controllable by some (specific) parameter?  (Or is
  // resource identifier object just degenerage case for "fields[xxx]=" (no
  // fields selected).)
  case class ResourceIdentifierObject(id: Some[ResourceId],  // optional for case we won't use
                                     `type`: TBD,
                                      meta: ResourceIdentifierObjectMetadata,
                                      `others?`: Arbitrary
                                     ) extends NonNullResourceObjectOrId
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
