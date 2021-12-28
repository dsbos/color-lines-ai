package com.us.dsb.explore.traits

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers._
import org.scalatest.matchers.should.Matchers._  // for "shouldNot compile"


class TraitMemberAccessExplTest extends AnyFunSuite {

  object Obj {
    val v = 1
    def m(): Unit = {}
    type T = Int
    object O
  }

  class Cls {
    val v = 1
    def m(): Unit = {}
    type T = Int
    object O
  }

  val inst = new Cls

  trait Trt {
    val v = 1
    def m(): Unit = {}
    type T = Int
    object O
  }


  test("???") {

    "val xt1 = Trt.v"     shouldNot compile  // "not found: value Trt"
    "val xt2 = Trt.m"     shouldNot compile  // "not found: value Trt"
    "val xt3: Trt.t = 1"  shouldNot compile  // "not found: value Trt"
    val xt4: Trt#T = 1
    "val xt5 = Trt.O"     shouldNot compile  // "not found: value Trt"
    "val xt6 = Trt#O"     shouldNot compile  // "';' expected but '#' found"

    "val xc1 = Cls.v"     shouldNot compile  // "not found: value Cls"
    "val xc2 = Cls.m"     shouldNot compile  // "not found: value Cls"
    "val xc3: Cls.t = 1"  shouldNot compile  // "not found: value Cls"
    val xc4: Cls#T = 1
    "val xc5 = Cls.O"     shouldNot compile  // "not found: value Cls"
    "val xc6 = Cls#O"     shouldNot compile  // "';' expected but '#' found"

    val xi1 = inst.v
    val xi2 = inst.m
    val xi3: inst.T = 1
    "val xi4: inst#T = 1" shouldNot compile  // "not found: type inst"
    val xi5 = inst.O
    "val xi6 = inst#O"    shouldNot compile  // "';' expected but '#' found"

    val xo1 = Obj.v
    val xo2 = Obj.m
    val xo3: Obj.T = 1
    "val xo4: Obj#T = 1"  shouldNot compile  // "not found: type Obj"
    val xo5 = Obj.O
    "val xo6 = Obj#O"     shouldNot compile  // "';' expected but '#' found"
  }

}
