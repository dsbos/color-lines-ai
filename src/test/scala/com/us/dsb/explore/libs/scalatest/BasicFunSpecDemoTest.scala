package com.us.dsb.explore.libs.scalatest

import org.scalatest.funspec.AnyFunSpec  // nested fn. lits.; describe/it

// (org.scalatest.matchers.should.Matchers._  can't be inside class because of inheritance)
import org.scalatest.matchers.should.Matchers._  // can't be inside because of inheritance

class BasicFunSpecDemoTest extends AnyFunSpec {
  describe("some aspect ...") {
    it("... do something") {
      1 shouldBe 1
      //fail()
    }

  }

}
