xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

assimilate:
- entity counts:  query total, entity/relationship total
- query parameters:  interpretation needs to look up entity types, attributes,
  relationships, etc.
- (query parameters . other)

Semi-sorted:
- High-level query types:
  - support like:  all users, user with specific ID (for all entity types)
  - maybe support traversing relationships (e.g., all domains related via
    users' containingDomain relationships from users)
    - Q:  Confirm:  Different from filtering domain collection via some
      expression detecting relationships from users?

  
- Metadata
  - Type metadata:
    ???
  - Instance metadata:
    ???



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
      ascending/descending; most datatype ordering obvious--enumerations by value
      string or some listing order?).
    - Presumably don't support sorting by related entities (e.g.,
      ...?sort=someRel/someAttr).



    
    
     
    

