package com.us.dsb.explore.strongtypes


object RefinedNewtypesExpl extends App {
  println("see RefinedNewtypesExplTest")

  object Types {
    import io.estatico.newtype.macros._
    import eu.timepit.refined.api.Refined
    import eu.timepit.refined.numeric.NonNegative
    import eu.timepit.refined.numeric.Positive
    import scala.language.implicitConversions


    /** Imagined query results offset. */
    @newtype case class Offset(raw: Int Refined NonNegative)

    /** Imagined query results limit, non-zero. */
    @newtype case class Limit(raw: Int Refined Positive)

    case class Query(offset: Offset, limit: Limit)

  }

}
