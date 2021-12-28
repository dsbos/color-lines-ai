package com.us.dsb.explore.strongtypes

// https://github.com/estatico/scala-newtype
import io.estatico.newtype.macros._


object CustomAccessorNewtypes extends App {
  // (Quiet warning "implicit conversion method opsThis should be enabled":)
  import scala.language.implicitConversions

  // (@newtype/newtypes must be in an object, since macro defines a type.)

  @newtype case class Plain(raw: Int)

  /** Newtype with default accessor suppressed. */
  @newtype case class SuppressedAccessor(private val raw: Int) {
    //val saved = raw  // "val definitions not supported, use def instead"

    // ?? TODO Q: How to access member when default accessor is suppressed?
    def get: Int =  {
      ???//this.raw  // "value raw is not a member of com.us.dsb.explore.strongtypes.CustomAccessorNewtype.SuppressedAccessor.Ops$newtype"
    }

  }

  Plain(0)
  Plain(raw = 0)
  Plain(0).raw

  SuppressedAccessor(1)
  SuppressedAccessor(raw = 1)
  SuppressedAccessor(1).get
}
