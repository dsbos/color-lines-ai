package com.us.dsb.explore.libs.scalatest

import org.scalatest.funspec.AnyFunSpec

// (org.scalatest.matchers.should.Matchers._  can't be inside class because of inheritance)
import org.scalatest.matchers.should.Matchers._
// can also mix in org.scalatest.matchers.should.Matchers

class BasicMatchers extends AnyFunSpec {

  describe(" ...") {
    it("equality flavors: === vs. equal vs. be, 'shouldXx' vs. 'should xx'") {

      // from https://www.scalatest.org/user_guide/using_matchers:
      0 should ===   (0)  // customizable equality and "can enforce type constraints" (?)
      0 should equal (0)  // customizable equality
      0 shouldEqual   0   // customizable equality, no parentheses required
      0 should be    (0)  // native equality, so fastest to compile
      0 shouldBe      0   // native equality, so fastest to compile, no parentheses required

      0 should !==  (1)
      0 should not be 1
      0 shouldNot be (1)     // (message: "was equal to")
      0 should not equal 1   // (message: "equaled")
      0 shouldNot equal (1)

      // Q: How/where does above "enforce type constraints" show up?:
      // 1 should === ("")  // note: no type warning
      // val x = 1
      // x should === ("")  // note: no type warning

      // assert(...) gets Scalatest's assert(...
      //assert(1 == 0)   // no values/expression shown
      //assert(1 === 0)  // differing values shown
      //assert(1 == "")  // differing values shown  (what's different?)
      //assert(1 === "") // differing values shown

      assert(1 != 0)
      assert(1 !== 0)
      assert(1 != "")
      assert(1 !== "")


      // 0 should == (0) // "overloaded method should with alternatives" - many

      //fail()

      // ?? TODO:  sameInstanceAs/theSameInstanceAs
      // ?? TODO:  StringNormalizations,
      // ?? TODO:  an [xxxException] should be thrownBy ..., etc.
      // ?? TODO:  shouldNot compile, shouldNot typeCheck
      0 should be < 7

      //??: "" should be a [String]

      new java.io.File("x") should be a 'file

      //https://www.scalatest.org/user_guide/using_matchers

    }

    describe("custom equality") {

      case class OneInt(raw: Int)
      case class OtherInt(raw: Int)

      it("uncustomized") {
        //OneInt(1) should ===(OtherInt(1))    // fails
        //OneInt(1) should equal(OtherInt(1))  // fails
        //OneInt(1) should be(OtherInt(1))     // fails
      }
      ignore("customized") {
        implicit val x1 = org.scalactic.Equality[OneInt](???)

        implicit val x2 = org.scalactic.Equality[OtherInt](???)

        OneInt(1) should ===(OtherInt(1))
        OneInt(1) should equal(OtherInt(1))
        OneInt(1) should be(OtherInt(1))

      }
    }

  }



}
