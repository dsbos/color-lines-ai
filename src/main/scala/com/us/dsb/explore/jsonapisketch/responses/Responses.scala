package com.us.dsb.explore.jsonapisketch.responses

import java.net.URI
import scala.collection.immutable.LinearSeq

object Responses extends App {

  type xxTBD = Unit
  type TBDString = String  // string, with value class TBD
  type xxDeferred = Unit

  type Arbitrary = Unit   // arbitrary JSON, except for attributes
  type AttributeValue = Unit   // arbitrary, but usually scalar, JSON value for attribute




  type MemberNameSyntax = String  // constrained ...
  type ResourceId = String    // ?? constrained?

  type DataTypeString = String  // MemberNameSyntax or no constraint?
  type ResourceTypeString = MemberNameSyntax  // member (field) name syntax
  type FieldName = MemberNameSyntax
  type AttributeName = FieldName
  type RelationshipName = FieldName

  //???? move some lower-level types to shared area (e.g., names that appear in
  // filter, sort, include, etc. parameters

  trait MetaObject  // just grouping trait; no commonality(?)
  
  // (probably unused/no JSON members)
  class JsonApiObjectMeta(val others: /*xx*/Arbitrary*) extends MetaObject

  trait DocMetadata extends MetaObject

  // primary data metadata (e.g., available attributes and types:
  class DataDocMetadata(val xxTBD: xxTBD) extends DocMetadata

  // if any "meta" with "error" member:
  class ErrorDocMetadata(val xxTBD: xxTBD) extends DocMetadata

  // would "meta" ever appear without "data" or "error?
  class MetaOnlyDocMetadata(val xxTBD: xxTBD) extends DocMetadata

  // if any metadata on links:
  class LinkMetadata(val others: Arbitrary*) extends MetaObject

  class ResourceObjectMetadata(val xxothers: /*xx*/Arbitrary*) extends MetaObject

  // if any "meta" on resource _identifier_ objects:
  class ResourceIdentifierObjectMetadata(val xxothers: /*xx*/Arbitrary*) extends MetaObject

  // probably target entity type, target cardinality
  class RelationshipObjectMetadata(val xxothers: /*xx*/Arbitrary*) extends MetaObject


  // https://jsonapi.org/format/1.1/#auto-id--link-objects:
  trait LinkValue  // link JSON "value" -> "string", "object", etc.
  case class LinkString(linkURL: URI) extends LinkValue
  case class LinkObject(href: URI,
                        rel: Some[TBDString], // link’s relation type.
                        describedby: Option[LinkValue],  // OpenAPI doc. link (for which cases?)
                        title: Option[TBDString],  // UI label for link? for which cases?
                        `type`: Option[TBDString],  // target's media type
                        hreflang: Either[TBDString, Seq[TBDString]], // string or string array; target's language(s)'s tag(s)
                        meta: LinkMetadata) extends LinkValue


  // ?? refine modeling re different sets of links for links objects at different
  //   places and for different cases (e.g., object vs. array for "data")
  case class LinksObject(self: Option[LinkValue],  // usually present
                         related: Option[LinkValue],  // present for all relationship objects?
                         first: Option[LinkValue], // group present whenever non-scala data?;
                         last: Option[LinkValue],  //   specific ones absent when unavailable
                         prev: Option[LinkValue],
                         next: Option[LinkValue],
                         about: Option[LinkValue], // for errors[].links
                         `type`: Option[LinkValue], // for errors[].links
                         `xxothers?`: LinkValue*)

  // no "id" or "type", nor "links" nor "relationships"
  // ("[foreign] keys SHOULD NOT appear as attributes")
  case class AttributesObject(members: Map[AttributeName, AttributeValue])

  case class ResourceIdentifierObject(id: Some[ResourceId],  // optional only for case we won't use
                                     `type`: ResourceTypeString,
                                      meta: Option[ResourceIdentifierObjectMetadata]  // probably none
                                     ) extends xxNonNullResourceObjectOrId
  case class xxNoResourceObject() extends xxResourceObjectOrId

  //?? review: "primary data" can be resource identifier objects as well as resource objects:
  trait xxxResourceObjectListOrIdList extends xxPrimaryData //?? maybe not just primary soon
  case class xxxResourceObjectList(tbd: /*xx*/Seq[xxTBD]) extends xxxResourceObjectListOrIdList
  case class xxxResourceIdObjectList(tbd: /*xx*/Seq[xxTBD]) extends xxxResourceObjectListOrIdList

  // https://jsonapi.org/format/1.1/#document-resource-object-linkage:
  trait ResourceLinkage
  class ArrayResourceLinkage(val nameThis: Seq[ResourceIdentifierObject]) extends ResourceLinkage
  class ScalarResourceLinage(val nameThis: Option[ResourceIdentifierObject]) extends ResourceLinkage


  //???? CONTINUE with relationships, resource linkage, etc.
  // - has at least of one links, data, and meta (part of spec that requires
  //   data member is only for creation)
  // - re having data _and_ links: xx if not using include/included, data's
  //   entity references via resource identifier objects can't be resolved to
  //   anything (data or URLs) without accessing server; providing links.related
  //   make it easy for client to get full data for any referenced entities--it
  //   doesn't have to construct a URL to get the entity; (but what about appending
  //   ID (from resource identifier object) to "related" URL--knows only to
  //   append, doesn't have to know starting segment (and relationship to type))
  // ???? research: xx data _and_ links? semantics?
  case class RelationshipObject(links: Some[LinksObject],  // with at least "self" or "related"
                                data: ResourceLinkage,
                                meta: RelationshipObjectMetadata
                                )

  case class RelationshipsObject(members: Map[RelationshipName, RelationshipObject])


  //??? clean up PrimaryData (re scalar/non-scalar, re resource vs. resource identifiers objects, re null/present)

  trait xxPrimaryData

  trait xxResourceObjectOrId extends xxPrimaryData //?? maybe not just primary soon
  trait xxNonNullResourceObjectOrId extends xxResourceObjectOrId
  case class ResourceObject(id: Some[ResourceId],  // optional only for case we won't use
                            `type`: DataTypeString,
                            attributes: Option[AttributesObject],  // present or absent if no attrs.?
                            relationships: Option[RelationshipsObject], // present or absent if no rels.?
                            links: Some[LinkValue],  // with "self" links--re constructing URLs for client
                            meta: Option[ResourceObjectMetadata] // anything for specific entity instance?
                            ) extends xxNonNullResourceObjectOrId

  //???: xxQ: xx Does this always identify a resource object in member included _and_
  //   having a "self" URL?  Or does client sometimes need to assemble URL string
  //   from entity type xx(re "user" for type xxand "GET /users/123")?
  //   - If primary data is resource _identifier_ object(s), is there alwways
  //     resource object(s) in response document?
  //???: xxQ: xxWhat determines whether primary data is resource objects or resource
  // identifier objects?  Undefined by JSON:API spec.?  Typically fixed per
  // service?  Typically controllable by some (specific) parameter?  (Or is
  // resource identifier object just degenerage case for "fields[xxxxx]=" (no
  // fields selected).)

  // https://jsonapi.org/format/1.1/#error-objects:
  case class ErrorSourceObject(pointer: Option[TBDString], // unused in our GET-only case
                               parameter: Option[TBDString],  // URI query parameter (name? name and value?)
                               header: Option[TBDString] // request header (name? name and value?)
                              )
  case class ErrorObject(
                        // at least one of:
                        id: Option[TBDString], // identifier for occurrence of error
                        links: Option[LinksObject], // links object, maybe with "about" and "type"
                        status: Option[TBDString], //  HTTP status code
                        code: Option[TBDString], // application-specific error code
                        title: Option[TBDString], // short, non-occurrence-specific summary of the problem
                        detail: Option[TBDString], // occurrence-specific report of the problem
                        source: Option[ErrorSourceObject],
                        meta: Option[ErrorDocMetadata],
                        )

  // https://jsonapi.org/format/1.1/#document-top-level:
  trait ResponseDoc



  // https://jsonapi.org/format/1.1/#document-jsonapi-object
  case class JsonApiObject(version: Option[String], // 1.1?
                           ext: Seq[URI],  // empty
                           profile: Seq[URI],  // empty
                           meta: Option[JsonApiObjectMeta]  // empty?
                          )


  case class DataResponseDoc(jsonApi: JsonApiObject,
                             meta: DataDocMetadata,
                             data: xxPrimaryData,   // would we ever return resource _ID_ objects?  controlled how?
                             links: Some[LinksObject],
                             included: Option[Seq[ResourceObject]] // present if requested
                             )


  case class ErrorResponseDoc(jsonapi: JsonApiObject,
                              meta: ErrorDocMetadata,
                              errors: Seq[ErrorObject],
                              `xxlinks?`: Option[LinksObject], // allowed without "data"?  meaning what?
                              `xxothers?`: xxTBD*
                             )
  // what would this be before
  case class xxMetaOnlyResponseDoc(jsonapi: JsonApiObject,
                                   xxmeta: MetaOnlyDocMetadata,
                                `xxlinks?`: Option[LinksObject], // allowed without "data"?  meaning what?
                                 `xxothers?`: xxTBD*
                                )

}
