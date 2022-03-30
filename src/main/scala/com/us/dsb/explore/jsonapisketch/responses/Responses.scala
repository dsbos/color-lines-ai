package com.us.dsb.explore.jsonapisketch.responses

import java.net.URI
import scala.collection.immutable.LinearSeq

object Responses extends App {

  type xxTBD = Unit
  type TBD = Unit
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
  // filter, sort, include, etc., parameters


  /** Contains flavors of "meta" objects within response document */
  object ResponseMetadata {

    trait MetaObject // just grouping trait; no commonality(?)

    // (probably unused/no JSON members)
    class JsonApiObjectMeta(val others: /*xx*/ Arbitrary*) extends MetaObject

    trait DocMetadata extends MetaObject // just grouping trait; no commonality(?)

    // primary data metadata (e.g., available attributes and types; count):
    // - entity type info, e.g.:
    //   - entity type name, possibly URL path segment
    //   - list of attributes (names, type info (simple to rich))
    //   - list of relationships (name, target entity type, cardinality, more?)
    // - count(s) (total entities, filtered entities)

    object DataDocMetadata {
      case class AttributeMetadata(name     : AttributeName,
                                   baseType : DataTypeString, // ??? what about if JSON object? named type?
                                   typeChain: Seq[DataTypeString]) // or just logical type

      case class RelationshipMetadata(name             : RelationshipName,
                                      targetType       : ResourceTypeString, // ?? what if multiple types
                                      targetCardinality: TBD) // scalar vs. non-scalar

      case class EntityTypeMetadata(entityTypeName   : ResourceTypeString,
                                    entityTypeSegment: TBDString,
                                    attributes       : Seq[AttributeMetadata],
                                    relationships    : Seq[RelationshipMetadata])
    }
    import DataDocMetadata._

    class DataDocMetadata(val entityType: EntityTypeMetadata,
                          val tbd       : TBD     // total count, filtered count
                         ) extends DocMetadata

    // if any "meta" with "error" member:
    class ErrorDocMetadata(val xxTBD: xxTBD) extends DocMetadata

    // would "meta" ever appear without "data" or "error?
    class MetaOnlyDocMetadata(val xxTBD: xxTBD) extends DocMetadata

    // if any "meta"  on links:
    class LinkMetadata(val others: Arbitrary*) extends MetaObject

    // if any "meta"  _inside_ individual resource objects:
    class ResourceObjectMetadata(val xxothers: /*xx*/ Arbitrary*) extends MetaObject

    // if any "meta" on resource _identifier_ objects:  ??? can they have links?
    class ResourceIdentifierObjectMetadata(val xxothers: /*xx*/ Arbitrary*) extends MetaObject

    // probably target entity type, target cardinality  ??? in relationship object? or included in entity-type metadata?; maybe counts but not entity-type data?

    class RelationshipObjectMetadata(val xxothers: /*xx*/ Arbitrary*) extends MetaObject

  }
  import ResponseMetadata._

  object Links {
    // https://jsonapi.org/format/1.1/#auto-id--link-objects:
    trait LinkValue // link JSON "value" -> "string", "object", etc.

    case class LinkString(linkURL: URI) extends LinkValue

    case class LinkObject(href       : URI,
                          rel        : Some[TBDString], // link’s relation type. (same as links member name)
                          describedby: Option[LinkValue], // OpenAPI doc. link (for which cases?)
                          title      : Option[TBDString], // UI label for link? for which cases?
                          `type`     : Option[TBDString], // target's _media_ type
                          hreflang   : Either[TBDString, Seq[TBDString]], // string or string array; target's language tag(s)

                          meta       : LinkMetadata // nothing?
                         ) extends LinkValue

    // ?? refine modeling re different sets of links for links objects at different
    //   places and for different cases (e.g., object vs. array for "data")
    case class LinksObject(self       : Option[LinkValue], // usually present
                           related    : Option[LinkValue], // present for all relationship objects?
                           first      : Option[LinkValue], // group present whenever non-scala data?;
                           last       : Option[LinkValue], //   specific ones absent when unavailable
                           prev       : Option[LinkValue],
                           next       : Option[LinkValue],
                           about      : Option[LinkValue], // for errors[].links
                           `type`     : Option[LinkValue], // for errors[].links
                           `xxothers?`: LinkValue*)
  }
  import Links.{LinksObject, LinkValue}


  case class ResourceIdentifierObject(id: Some[ResourceId],  // optional only for case we won't use
                                     `type`: ResourceTypeString,
                                     // (seemingly) no "links:
                                      meta: Option[ResourceIdentifierObjectMetadata]  // probably none
                                     ) extends xxxResourceIdObjectOrObjects
  case class xxNoResourceIdObject() extends xxxResourceIdObjectOrObjects
  case class ResourceIdObjects(tbd: Seq[ResourceIdentifierObject]) extends xxxResourceIdObjectOrObjects




//  // https://jsonapi.org/format/1.1/#document-resource-object-linkage:
//  trait xxResourceIdObjectOrObjects // resource linkage?
//  class ArrayResourceLinkage(val nameThis: Seq[ResourceIdentifierObject]) extends xxResourceIdObjectOrObjects
//  class ScalarResourceLinkage(val nameThis: Option[ResourceIdentifierObject]) extends xxResourceIdObjectOrObjects


  //???? CONTINUE with relationships, resource linkage, etc.
  // - has at least of one "links", "data", and "meta" (part of spec that requires
  //   data member is only for creation)
  // - re having "data" _and_ "links":  if not using include/included, data's
  //   entity references via resource identifier objects can't be resolved to
  //   anything (data or URLs) without accessing server; providing links.related
  //   make it easy for client to get full data for any referenced entities--it
  //   doesn't have to construct a URL to get the entity; (but what about appending
  //   ID (from resource identifier object) to "related" URL--knows only to
  //   append, doesn't have to know starting segment (and relationship to type))
  // ???? research:  data _and_ links? semantics?
  case class RelationshipObject(links: Some[LinksObject],  // with at least "self" or "related"
                                data: xxxResourceIdObjectOrObjects,
                                meta: RelationshipObjectMetadata
                                )


  // no "id" or "type", nor "links" nor "relationships"
  // ("[foreign] keys SHOULD NOT appear as attributes")
  case class AttributesObject(members: Map[AttributeName, AttributeValue])

  case class RelationshipsObject(members: Map[RelationshipName, RelationshipObject])


  //??? clean up PrimaryData (re scalar/non-scalar, re resource vs. resource identifiers objects, re null/present)

  // in https://jsonapi.org/format/1.1/#document-top-level:
  // superclass for primary data (but ... subclass occurrences aren't all primary data)
  trait xxxResourceOrResourceIdObjectOrObjects
  // "data":
  // - scalar or non-scalar (JSON object (or null) or JSON list (possibly empty))
  // - all resource objects or all resource identifier objects (not mixed)
  // ????assim:  resource _identifier_ objects when primary data in a relationship
  //   object; probably never otherwise (just resource objects otherwise)

  // regular data: primary data if not relationship; included's data?
  // ... one resource object, null, or array of object (possibly empty)
  trait xxxResourceObjectOrObjects extends xxxResourceOrResourceIdObjectOrObjects

  // ... one RID, null, or array of RIDs (possibly empty)
  trait xxxResourceIdObjectOrObjects extends xxxResourceOrResourceIdObjectOrObjects

  case class ResourceObject(id: Some[ResourceId],  // optional only for case we won't use
                            `type`: DataTypeString,
                            attributes: Option[AttributesObject],  // present or absent if no attrs.?
                            relationships: Option[RelationshipsObject], // present or absent if no rels.?
                            links: Some[LinkValue],  // with "self" links--re constructing URLs for client
                            meta: Option[ResourceObjectMetadata] // anything for specific entity instance?
                            ) extends xxxResourceObjectOrObjects
  case class NoResourceObject() extends xxxResourceObjectOrObjects
  case class ResourceObjects(tbd: Seq[ResourceObject]) extends xxxResourceObjectOrObjects


  type ResourceLinkage = xxxResourceIdObjectOrObjects  // name from .../1.1/#document-resource-object-linkage
  type PrimaryData = xxxResourceOrResourceIdObjectOrObjects



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


  // "data" - primary data- resource object(s) when primary data is normal
  //    entity(-ies), but resource _identifier_ object(s) when primary data is
  //    relationship object;
  //   (Do we have any need to serve relationship objects?  (Do we need to
  //   provide self links for relationships objects?))
  case class DataResponseDoc(jsonApi: JsonApiObject,
                             meta: DataDocMetadata,
                             data: xxxResourceOrResourceIdObjectOrObjects,
                             links: Some[LinksObject],
                             included: Option[Seq[ResourceObject]] // present if requested
                             ) extends ResponseDoc


  case class ErrorResponseDoc(jsonapi: JsonApiObject,
                              meta: ErrorDocMetadata,
                              errors: Seq[ErrorObject],
                              // spec.: allows "links', but "related to the primary data"
                              ) extends ResponseDoc

  // what would this be for?:
  case class MetaOnlyResponseDoc(jsonapi: JsonApiObject,
                                 meta: MetaOnlyDocMetadata,
                                 // spec.: allows "links', but "related to the primary data"
                                 ) extends ResponseDoc

}
