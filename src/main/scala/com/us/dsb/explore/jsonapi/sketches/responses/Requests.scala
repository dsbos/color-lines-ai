package com.us.dsb.explore.jsonapi.sketches.responses

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

  case class EntityId(raw: String) extends AnyVal

  trait EntityType
  case object User extends EntityType
  // Group, Domain, etc.

  trait Relationship
  case object `User.groups` extends Relationship
  case object `User.primaryGroup` extends Relationship
  // *.domain, etc.


  def getEntityTypeForSegment(entitiesSegment: String): Either[String, EntityType] = {
    // initial hack implementation only
    val result =
      entitiesSegment match {
        case "users" => Right(User)
        case unknown => Left(s"Unknown entity-type-related segment '$unknown'")
      }
    result
  }

  def getEntityTypeRelationshipForSegment(entityType: EntityType,
                                          relationshipSegment: String
                                         ): Either[String, Relationship] = {
    // initial hack implementation only
    val result: Either[String, Relationship] = {
      (entityType, relationshipSegment) match {
        case (User, "groups") => Right(`User.groups`)
        case (User, "primaryGroup") => Right(`User.primaryGroup`)
        case (entityType, relationship) =>
          Left(s"Entity type $entityType has no `$relationship` relationship.")
        //case _ => ???
      }
    }
    result
  }

  trait RequestPath2
  case object VirtualRootObjectPath2 extends RequestPath2
  case class EntitiesByTypePath2(entityType: EntityType) extends RequestPath2  // ?? single/multiple?
  case class EntityByIdPath2(entityType: EntityType, entityId: EntityId) extends RequestPath2  // ?? single/multiple?

  case class SimpleRelationshipObjectPath2(singleEntityPath: EntityByIdPath2,
                                           relationship: Relationship
                                          ) extends RequestPath2
  case class SimpleRelatedDataPath2(singleEntityPath: EntityByIdPath2,
                                    relationship: Relationship
                                   ) extends RequestPath2

  def interpretPath(rawAbsPath: String): Either[String, RequestPath2] = {
    require(rawAbsPath.startsWith("/"))

    val segments = rawAbsPath.split("/").drop(1).toList
    def NIY = Left(s"Not implemented yet: '$rawAbsPath'")
    val result: Either[String, RequestPath2] =
      segments match {
        case Nil =>
          Right(VirtualRootObjectPath2)

        case entitiesSegment :: Nil =>
          getEntityTypeForSegment(entitiesSegment)
              .map(entityType => EntitiesByTypePath2(entityType))

        case entitiesSegment :: entityIdSegment :: Nil =>  // ???? fold together (re nested EntityByIdPath2)
          getEntityTypeForSegment(entitiesSegment)
                        .map(entityType => EntityByIdPath2(entityType, EntityId(entityIdSegment)))

        case entitiesSegment :: entityIdSegment :: tail =>  // ???? fold together (re nested EntityByIdPath2)
          tail match {
            case "relationships" :: relationshipSegment :: Nil =>
              val refactorThis =
                        getEntityTypeForSegment(entitiesSegment)
                                      .map(entityType => EntityByIdPath2(entityType, EntityId(entityIdSegment)))
              refactorThis.flatMap { entityPath =>
                getEntityTypeRelationshipForSegment(entityPath.entityType,
                                                    relationshipSegment)
                    .map(relationship => SimpleRelationshipObjectPath2(entityPath,
                                                                       relationship))
              }
            case relationshipSegment :: Nil =>
               val refactorThis =
                         getEntityTypeForSegment(entitiesSegment)
                                       .map(entityType => EntityByIdPath2(entityType, EntityId(entityIdSegment)))
              refactorThis.flatMap { entityPath =>
                 getEntityTypeRelationshipForSegment(entityPath.entityType,
                                                     relationshipSegment)
                     .map(relationship => SimpleRelatedDataPath2(entityPath,
                                                                        relationship))
               }
            case chainTail@_ =>
              NIY
          }
      }
    println(s"'$rawAbsPath' -> $result")
    result
  }

  val testStringPaths = List[String](
    //"badAbsolutePath",
    "/",
    "/users",
    "/unknown",
    "/users/user1",
    "/users/user1/relationships/unknown",
    "/users/user1/relationships/groups",
    "/users/user1/relationships/primaryGroup",
    "/users/user1/groups",
    "/users/user1/primaryGroup",
    "/users/user1/primaryGroup/domain"
  )
  val testInterpretedPaths = testStringPaths.map(interpretPath(_))


  case class RequestURL(
                       path: RequestPath2,
                       tbd: Unit
                       )

}
