package com.us.dsb.explore.strongtypes


import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._  // for "shouldNot compile"


class BasicNewtypesExplTest extends AnyFunSpec {
  import BasicNewtypes.Types._

  describe("newtypes with same structure should not be compatible:") {

    it("- primitive") {
      Primitive(0): Primitive
      "Primitive(0): SameShapePrimitive" shouldNot typeCheck
    }

    it("- collection/nested") {
      Deeper(List(Nested(Primitive(0)))): Deeper
      "Deeper(List(Nested(Primitive(0)))): SameShapeDeeper" shouldNot typeCheck
    }

  }

}