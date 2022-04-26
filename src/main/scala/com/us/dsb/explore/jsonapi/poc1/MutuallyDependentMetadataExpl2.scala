package com.us.dsb.explore.jsonapi.poc1

// Prototypes having mutually dependent types with circularly linked instances,
// such as entity type and relationship, where entity type has (points to) zero
// or more relationships, and relationship has two associated entity types
// (source and target).  (Or model with mutual dependency at a different place,
// e.g., something with "relationship// halves.")

object MutuallyDependentMetadataExpl2 extends App {

  // Prototypes client code's (read-only) view of mutually dependent metadata:

  /** Metadata interface to client code. */
  object Interface {
    trait Type {
      def relationships: Set[Relationship]  // types can have relationships
    }
    trait Relationship {
      def sourceType: Type                  // relationships have types
      def targetType: Type
    }
  }

  // Immutable plain classes, effectively patching via by-name parameters--
  // orders of declaration and reference don't matter:
  object Implementation {

    class Type(_relationships: => Set[Interface.Relationship])
        extends Interface.Type {
      def relationships: Set[Interface.Relationship] = _relationships
      println("Type: relationships = " + relationships)
    }

    class Relationship(_sourceType: => Type,
                       _targetType: => Type) extends Interface.Relationship {
      def sourceType: Type = _sourceType
      def targetType: Type = _targetType
      println("Relationship: _sourceType = " + _sourceType)
    }

    val rel1: Relationship = new Relationship(type1, type1)
    val type1: Type = new Type(Set(rel1))
    println("NameThis.1:")
    println("- a1.targetType.relationships = " + rel1.targetType.relationships)
    println("- type1.relationships.head = " + type1.relationships.head)
    println("- type1.relationships.head.sourceType = " + type1.relationships.head.sourceType)
    println("- type3.relationships.head.sourceType.relationships = " + type3.relationships.head.sourceType.relationships)

    val type2: Type = new Type(Set(rel2))
    val rel2: Relationship = new Relationship(type2, type2)
    println("NameThis.2:")
    println("- a2.targetType.relationships = " + rel1.targetType.relationships)
    println("- rel2.targetType.relationships = " + rel2.targetType.relationships)
    println("- rel2.targetType.relationships.head.sourceType = " + rel2.targetType.relationships.head.sourceType)
    println("- rel2.targetType.relationships.head.sourceType.relationships = " + rel2.targetType.relationships.head.sourceType.relationships)

    object rel3 extends Relationship(type3, type3)
    object type3 extends Type(Set(rel3))
    println("NameThis.3:")
    println("- a3.targetType.relationships = " + rel3.targetType.relationships)
    println("- type3.relationships.head = " + type3.relationships.head)
    println("- type3.relationships.head.sourceType = " + type3.relationships.head.sourceType)
    println("- type3.relationships.head.sourceType.relationships = " + type3.relationships.head.sourceType.relationships)

    object type4 extends Type(Set(rel4))
    object rel4 extends Relationship(type4, type4)
    println("NameThis.4:")
    println("- a4.targetType.relationships = " + rel4.targetType.relationships)
    println("- type4.relationships.head = " + type4.relationships.head)
    println("- type4.relationships.head.sourceType = " + type4.relationships.head.sourceType)
    println("- type4.relationships.head.sourceType.relationships = " + type4.relationships.head.sourceType.relationships)
  }
  Implementation

}