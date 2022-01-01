package com.us.dsb.explore.strongtypes.refined

import com.sun.xml.internal.ws.api.server.InstanceResolverAnnotation
import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import scalaz.unused

import scala.util.Try


/**
 * Least-magic/most-explicit forms for refined types.
 * (No `eu.timepit.refined.auto.`... imports).
 */
object BasicMostExplicitDemo extends App {

  ////////////////////////////////////////
  // 1. Partial type safety:

  // ********** added safety:
  //val pos1: PosInt = 42  // "type mismatch; found: Int... required: ...PosInt"
  val pos2: PosInt = PosInt(42)
  // val int1: Int = pos2  // "type mismatch; found: ...PosInt ... required: Int"
  val int2: Int = pos2.value


  // ********** only _partial_ safety:
  type OtherPositiveInt = Int Refined Positive
  object OtherPositiveInt extends RefinedTypeOps.Numeric[PosInt, Int]
  val oth2: OtherPositiveInt = OtherPositiveInt(42)

  val oth3: OtherPositiveInt = pos2  // <== NO type mismatch
  val pos3: PosInt = oth2            // <== NO type mismatch


  ////////////////////////////////////////
  // 2. Compile-time validation when wrapping literals (static-enough expressions):

  // ********** Basic:
  PosInt(42)
  // PosInt(-13)  // Predicate failed: (-13 > 0).

  // Constrain with either type alias or type "expression":
  PosInt(42): PosInt
  PosInt(42): Int Refined Positive
  // no (Int Refined Positive)(42)
  // ?? Q: Can we use "Int Refined Positive" in static wrapping?

  // Custom type:
  type MyType = Int Refined Positive
  object MyType extends RefinedTypeOps.Numeric[PosInt, Int]

  MyType(42)
  // MyType(-13)  // Predicate failed: (-13 > 0).


  ////////////////////////////////////////
  // 3. Unwrapping:
  val v1: PosInt = PosInt(42)

  //////////
  // ********** Unwrapping:
  v1.value: Int  // @annotation.nowarn // ?? TODO: Why doesn't suppression work?


  ////////////////////////////////////////
  // 4. Run-time validation:

  val pos = 42
  val not = -13
  //PosInt(pos)  // "compile-time refinement only works with literals"

  import eu.timepit.refined.api.RefType.applyRef
  import eu.timepit.refined.refineV

  // ********* Wrapping dynamic expressions, returning Either:

  // a: RefType's applyRef: takes whole refined type:
  val form1ResultGood = applyRef[Int Refined Positive](pos)
  val form1ResultBad = applyRef[Int Refined Positive](not)
  val form1ResultBad2 = applyRef[PosInt](not) // form with type alias

  // b: refined's refineV: takes refinement only:
  val form2ResultGood = refineV[Positive](pos)
  val form2ResultBad = refineV[Positive](not)

  // c: Whole refine types' companion objects' from:
  PosInt.from(pos)
  PosInt.from(-not)

  // ********* Wrapping dynamic expressions, throwing IllegalArgumentException if invalid:

  // a: Whole refine types' companion objects' unsafeFrom:
  Try(PosInt.unsafeFrom(pos))
  Try(PosInt.unsafeFrom(not))

  // ?? Q:  What is ~generic equivalent?

}
