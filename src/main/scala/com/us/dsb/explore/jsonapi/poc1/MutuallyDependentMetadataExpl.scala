package com.us.dsb.explore.jsonapi.poc1

// Prototypes having mutually dependent types with circularly linked instances,
// such as entity type and relationship, where entity type has (points to) zero
// or more relationships, and relationship has two associated entity types
// (source and target).  (Or model with mutual dependency at a different place,
// e.g., something with "relationship// halves.")

object MutuallyDependentMetadataExpl extends App {

  // Prototypes client code's (read-only) view of metadata:
  object Interface {

    trait A {
      def b: B
    }
    trait B {
      def a: A
    }
  }

  // The following prototypes options for creating metadata:

  // Note:  Can't use by-name parameter on case class.
  // (Note:  Forward references in block-expression braces wouldn't even compile.)

  // Doesn't work:  Case classes:
  object CaseClasses {
    case class A(b: B) extends Interface.A
    case class B(a: A) extends Interface.B
    val a1: A = A(b1)  // b1's current null gets put in the A
    val b1: B = B(a1)
    println("CaseClasses:")
    // NullPointerException:  println("- a1.b.a = " + a1.b.a)
  }
  CaseClasses

  // Doesn't work:  Immutable plain classes, but still like case classes:
  object ImmutablePlainClasses {
    class A(_b: B) extends Interface.A {
      def b: B = _b
    }
    class B(_a: A) extends Interface.B {
      def a: A = _a
    }
    val a1: A = new A(b1)
    val b1: B = new B(a1)
    println("ImmutablePlainClasses:")
    // NullPointerException:  println("- a1.b.a = " + a1.b.a)
  }
  ImmutablePlainClasses

  // Works, but error-prone:  Mutable plain classes, patched manually:
  object MutablePlainClassesManualPatching {
    class A(var _b: B) extends Interface.A {
      def b: B = _b
    }
    class B(_a: A) extends Interface.B {
      def a: A = _a
    }
    val a1: A = new A(null)  // mustn't forget to patch null later
    val b1: B = new B(a1)
    a1._b = b1                    // might be far from a1
    println("MutablePlainClassesManualPatching:")
    println("- a1.b.a = " + a1.b.a)
  }
  MutablePlainClassesManualPatching

  // Works, error-prone if objects:  Mutable plain classes, patched in constructor:
  object MutablePlainClassesBPatchesA {
    class A(var _b: B) extends Interface.A {
      def b: B = _b
    }
    class B(_a: A) extends Interface.B {
      a._b = this  // <<== creating B updates/patches A to B
      def a: A = _a
    }
    val a1: A = new A(null)
    val b1: B = new B(a1)
    println("MutablePlainClassesBPatchesA:")
    println("- b1.a.b = " + b1.a.b)
    println("- a1.b.a = " + a1.b.a)

    // BRITTLE:  Success or NullPointException depend on order of references:
    object a2 extends A(b2)
    object b2 extends B(a2)
    println("MutablePlainClassesBPatchesA.2:")
    println("- b2.a.b = " + b2.a.b)
    println("- a2.b.a = " + a2.b.a)
  }
  MutablePlainClassesBPatchesA

  // Works, robust(?):  Mutable plain classes, effectively patched via by-name use.
  object MutablePlainClassesByNameParameter {
    class A(_b: => B) extends Interface.A {  // <<== saves caller B reference for later
      def b: B = _b
    }
    class B(_a: A) extends Interface.B {
      def a: A = _a
    }
    val a1: A = new A(b1)
    val b1: B = new B(a1)
    println("MutablePlainClassesByNameParameter:")
    println("- a1.b.a = " + a1.b.a)
  }
  MutablePlainClassesByNameParameter


  //???? UPDATE following:  Mixed version is order-dependent.  Need have by-name
  // parameters on both (all?) parts.

  // *** PROBABLE PATTERN:  Mostly case classes (or whatever's convenient for
  // building up most of metadata), with occasional multiple plain classes for
  // cutting dependency loops (via by-name parameters or patching):

  // Works, robust(?), more simple code:  Immutable mixed case and plain classes,
  // effectively patched via by-name use.
  object MutableMixedClassesByNameParameter {
    class A(_b: => B) extends Interface.A {
      def b: B = _b
    }
    // Bulk can be simple case classes.
    case class B(a: A) extends Interface.B  // <<== mostly simple case class(es)

    val a1: A = new A(b1)
    val b1: B = new B(a1)
    println("MutableMixedClassesByNameParameter.1:")
    println("- a1.b.a = " + a1.b.a)

    object a2 extends A(b2)
    object b2 extends B(a2)
    println("MutableMixedClassesByNameParameter.2:")
    println("- a2.b.a = " + a2.b.a)
    println("- b2.a.b = " + b2.a.b)
  }
  MutableMixedClassesByNameParameter

  // B-patches-A pattern should work too (even with case class B)

}