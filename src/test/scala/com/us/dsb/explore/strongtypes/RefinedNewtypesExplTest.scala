package com.us.dsb.explore.strongtypes

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._  // for "shouldNot compile"


class RefinedNewtypesExplTest extends AnyFunSpec {
  import RefinedNewtypesExpl.Types._

  describe("Newtypes with a refined-type member:") {

    describe("should reject invalid values:") {
      import eu.timepit.refined.auto.autoRefineV
      describe("refinement 'Int with NonNegative':") {
        it("- should accept 1") {
          Offset(1)
        }
        it("- should accept 0") {
          Offset(0)
        }
        it("- should reject -1") {
          "Offset(-)" shouldNot typeCheck
        }
      }
    }

    describe("constructor/etc. with newtype-type formal arguments should:") {
      import eu.timepit.refined.auto.autoRefineV
      val offset = Offset(0)
      val limit = Limit(1)
      def query(limit: Limit, offset: Offset) = ()

      it("accept right-type actual arguments") {
        Query(offset, limit)
        query(limit, offset)
      }
      it("reject wrong-type actual arguments (at compilation time)") {
        "Query(limit, offset)" shouldNot typeCheck

        "Query(limit, limit)" shouldNot typeCheck
        "query(limit, limit)" shouldNot typeCheck

        "Query(offset, offset)" shouldNot typeCheck
        "query(offset, offset)" shouldNot typeCheck

        "query(offset, limit)" shouldNot typeCheck
      }
      it("reject valid values of non-wrapped underlying types")  {
        "Query(1, 1)" shouldNot typeCheck
        "query(1, 1)" shouldNot typeCheck
      }
    }
  }

}