xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


assimilate:
- chain(s) of dependencies between things:
  - (dependencies across things (e.g., JSON subtrees) that seem somewhat
    independent)
  - type-metadata chain:  to datatypes, from entity types derived from
    query--primary data's type or types, included data's type(s), maybe related
    data's type(s) in relationships); depends on "fields"
  - included-data chain:  to included resource objects, from resource ID objects
    in primary data (including relationships) and from other included objects
    - (depends on "included" and "fields")
    - (how does that map to SQL (without querying for each referring object)?)
- query parameters:  interpretation needs to look up entity types, attributes,
  relationships, etc.
- (query parameters . other)
- multi-entity-type relationships (e.g., groups members (e.g., users, computers,
  other groups, if as one relationship)
  - entity type handled fine in "type" member
  - need some kind of union entity type (e.g., "user or computer or group")
  - how would query map to SQL and return resource objects of different types?
    - multiple single-join queries?
    - one query using multiple joins getting different tables' data into
      separate groups of columns, UNIONED, with each row mapped to entity type
      per which column group filled in
    - multiple joins?
  - probably just reserve "syntax" to represent union types (e.g., entity
    type names can map to either regular entity type or union entity type)
- what was relationships choice re avoiding "user" vs. "users" in UI re
  references?
- mapping to DB queries (~collected)
  - adding customer account ID (not in logical entity model)
  - attributes:  normally to columns, but maybe to expressions
  - entity collections:  normally to tables, but maybe to table expressions
    (e.g., JOINs)
  - filtering:  narrows down WHERE clause
    - What about filtering via exists/forall on relationships?  Just
      SQL subexpression, or change to form of query?
  - relationships:
    - ... "simple" vs. association table ...
    - ... ("included", JOIN stuff, etc.
- error handling:
  - general
  - JSON:API-specific (e.g,. "errors")
- HTTP server connection:
  - general
  - Don't _manually_ create _multiple_ HTTP-framework "routes" for available
    paths:
    - Create one ~high-level route, get path tail, interpret path segments.
    - Maybe, use metadata to create multiple routes:
      - (If framework can do something useful when given individual routes/paths
        (showing what's available? better logging?))
  



Semi-sorted:
- High-level query types:
  - support like:  all users, user with specific ID (for all entity types)
  - maybe support traversing relationships (e.g., all domains related via
    users' containingDomain relationships from users)
    - Q:  Confirm:  Different from filtering domain collection via some
      expression detecting relationships from users?
- Base query synax in URL path:
 - 

  
- Result metadata
  - Type metadata:
    - Entity-type metadata:
      - List/describe fields (attributes and relationship)
        - so client can know what "fields" takes.
      - (Reserve "syntax" for union types re multi-entity-type relationships.)
    - Datatype metadata:
      - At least phystical and logical types (e.g., "string" and "entityName" ).
      - Maybe chain of subtyping, like "string" -> "entityNane" -> "userName".
        - (UI uses most-specific one it knows. E.g., map types to HTML element
          classes; whichever classes CSS covers affect rendering.)
      - "Obvious" types:  string, int/long, float/double, boolean, etc.
      - Enumeration types:
        - (Presumably to UI can help user select value for filter expressions?)
        - Q:  Ordered by enumerator name or by order given somewhere?  (See
          pseudo-submembern_ note.)
      - Timestamps:
        - Maybe JSON object carrying both milliseconds offset and timestamp string.
          - Maybe simply ~hard-coded (defined) that way.
          - Maybe instance of class of types for JSON object structures? (Can
            probably for allow adding that later.)
          - (See pseudo-submember note.)
      - Possibly use submember syntax to refer to psuedo-members to request
        ~variations:
        - e.g., in "fields", "created.epochMillis" vs. "created.stringForm" (vs.
          just "created" to get JSON object with both)
        - e.g., in "sort", "enumAttr.logicalOrder" vs. "enumAttr.stringOrder"


  - Instance metadata:
    - Entity counts:
      - Query total (without paging; after filtering).
        - (For, e.g., showing number of pages.)
      - Entity/relationship list total (before filtering).
        - (For showing ...)

- "Model" metadata:
  - (The model-specific metadata defining our logical entities and mappings
    to the data store. "driver metadata"? "control ..."? "master .."?
    "mapping ..."? )
  - Contains metadata selected to make result metadata (e.g., result's specfic
    entity type(s)), e.g., entity types, data types.
  - Contains mappings to/from data store:
    - E.g., an entity type's corresponding table (or table expression?).
    - E.g., an attribute's corresponding column (or other expression?).
    - E.g., a relationship's corresponding query components.
     



- Relationships:
  - Mapping from logical relationships to database representation:
    - Represented in model metadata



- generally, things in top-level metadata vs. things in various levels' metadata

- Query parameter aspects:
  - Paging:
    - assimilate:  threshold for paging by default?
    - assimilate:  any hard limits on page size/limit?
    - (Not for single-entity queries.)
    - For:  primary data.  Q: What about "included" and relationship collections?
    - Accept which forms: 1. offset and limit, 2. page size and number, or
      3. either (or 4. somethine else)?
      - if page size and number/offset, client can't get arbitrary range (e.g.,
        page size 10, want 8 items in range 17 to 24)
      - if offset and limit, might complicate generating first/prev/next/last
        links, especially if client sends offset that is not multiple of limit
        or changes limit
        - maybe just ignore irregularities (Prev then Next isn't "identity"
        traversal)
    - Provide first/prev/next/last links:
      - Only when paged?
      - "When available";  Are "first" and "last" always available?  Is
        availability of "prev" and "next" clear?
    
    - TBD:  support UI's display of paging state and available actions
      - (e.g., state like current page number and/or offsets, and actions like
        "page 1", "page 2", "page 3" links)
      - probably have entity count in metadata (top-level only? relationships'
        too?)
      - probably have current page/offset/etc. numbers in metadata

  - Sorting:
    - (Primary data only.  Not order in "included" or in relationship objects.)
    - Support basic sorting (by attributes only, one or multiple,
      ascending/descending; most datatype ordering obvious--enumerations by
      value string or some listing order?).
    - Presumably don't support sorting by related entities (e.g.,
      ...?sort=someRel/someAttr).

  - Included data:
    - (Depends on supporting relationships.)
    - TBD:  Supported?  How much?
      - From just one relationships or multiple?
        - (Just one seems arbitrary limitation from usage side, but possibly
          simpler to implement.)
      - One-hop or multiple-hop?
      - (Any other levels/features?)
    - No duplicate resource objects, even if entity multiply referenced.
    


    
    
     
    



