package com.us.dsb.explore.jsonapisketch.responses

object Requests extends App {
  /*
  https://rationemllc.atlassian.net/wiki/spaces/CA/pages/3123085352/URL+Paths+request+cases+path+forms+related+aspects

    - /  (possibly)
    - /things
    - /things/1
    - /things/1/relationships/someRel
    - /things/1/someRel
    - maybe:
      - /things/1/someToOneRel/relationships/someRel
      - /things/1/someToOneRel/someRel
      - /things/1/someToOneRel/anotherToOneRel/relationships/someRel
      - /things/1/someToOneRel/anotherToOneRel/someRel
      - etc. (longer chains)

    - parsing by number of segments first:
      - 0 segments - top-level object
      - 1 segment  - all entities of type (look up entity type from segment)
      - 2 segments - single entity by type and ID (no further lookup)
      - 3+ segments:
        - first two - single entity by type and ID
        - if second to last is "relationships" - relationship object for some entity:
          - any between second and second to last must be names of chained to-1
            relationships (look up each in entity type of its prefix)
        - otherwise (not "relationships") - related data for relationship from some entity:
          - any between second and last must be names of chained to-1 relationships
        - last is final relationship name (look up in entity type of prefix0
    - parsing by incrementally ~assimilating segments
      - start ~state:  at top path level, no "data specification" yet
      - check whether first segment:
        - if none:    set data to virtual top-level object; exit parsing
        - if exists:  is entity-type segment; set current data to entity collection of type
      - check whether second segment
        - if none:  exit parsing with final data being entity-type collection
        - if exists:  is entity ID; wrap current data with get-by-ID step
      - loop until exit: (right place?)
        - check wheher current segment:
          - if none:  exit parsting, with current data setting
          - if not "relationships": is relationship name; look up relationship
            in current data's entity type; wrap current data in
            get-related-data step
          - if "relationships": get next segment as relationship name; look up
            relationship in current data's entity type; wrap current data in
            get-relationship-object step
            - or maybe wrap in partial step to be completed in next loop iteraton
      - (checking for to-1 relationships in prefix of chain isn't addressed yet)




   */

  val entityTypeNames    = List[TBD]("user",  "domain",  "group")
  val entityTypeSegments = List[TBD]("users", "domains", "groups")


  type TBD = String

  trait RequestPath
  trait SingleEntityPath extends RequestPath
  trait EntityCollectionPath extends RequestPath
  // ??? relationship object as SingleEntityPath or separate (dedicated) case?


  case object VirtualRootObjectPath extends SingleEntityPath

  case class BaseEntityCollectionPath(entityTypeSegment: TBD) extends EntityCollectionPath


  case class EntityByIdPath(entityTypeSegment: TBD, entityid: TBD) extends SingleEntityPath
  // or in terms of nested EntityCollectionPath?

  // at earlier point, don't know yet that prefix relationship is to-1 (to
  //   be valid prefix) and (for non-/relationships/ case) whether final
  //   relationship is to-1 (to extend SingleEntityPath or not)
  //
  case class RawRelationshipObjectPath(sourceEntityPath: RequestPath,
                                       notYetResolvedRelationshipName: TBD) extends RequestPath
  case class RawRelatedDataPath(sourceEntityPath: RequestPath,
                                notYetResolvedRelationshipName: TBD) extends RequestPath


  // at later point, have limited to valid (single-entity) prefix, and identifid
  //   whether final relationship is to-1 (to extend SingleEntityPath or not)

  case class RelationshipObjectPath(sourceEntityPath: SingleEntityPath,
                                    relationshipName: TBD
                                   ) extends RequestPath  //???? single object, but not _entity_


  case class ToOneRelatedDataPath(sourceEntityPath: SingleEntityPath,
                                  toNRrelationshipName: TBD) extends SingleEntityPath
  case class ToNRelatedDataPath(sourceEntityPath: SingleEntityPath,
                                toOneRelationshipName: TBD) extends EntityCollectionPath


  val samples = List[RequestPath](
      VirtualRootObjectPath,
      BaseEntityCollectionPath("users"),  // (not necessarily same as type string, e.g. "user")
      EntityByIdPath("domains", "QOMPLX.COM"),

      RawRelationshipObjectPath(
        sourceEntityPath = EntityByIdPath("domains", "QOMPLX.COM"),
        "notYetResolvedRelationship"),
      RawRelatedDataPath(
        sourceEntityPath = EntityByIdPath("domains", "QOMPLX.COM"),
        "notYetResolvedRelationship"),


      RelationshipObjectPath(
        sourceEntityPath = EntityByIdPath("users", "1"): SingleEntityPath,
        "anyCardinalityRelationship": TBD),

      ToOneRelatedDataPath(
        sourceEntityPath = EntityByIdPath("users", "1"): SingleEntityPath,
        "userToOneDomainRelationship"),
      ToNRelatedDataPath(EntityByIdPath("users", "1"),
                         "userToGroupsRelationship")

  )
  println(samples.mkString("Samples:\n- ", "\n- ", ""))





  case class RequestURL(
                       path: RequestPath,
                       tbd: Unit
                       )

}
