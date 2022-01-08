package com.us.dsb.explore.enums.enumeratum

import enumeratum.Enum
import enumeratum.EnumEntry


object EnumeratumExpl extends App {


  // Basic use:
  // - type:
  sealed trait Basic extends EnumEntry
  object Basic extends Enum[Basic] {

    // - enumerators:
    case object One extends Basic
    case object Two extends Basic

    // - automatic listing of enumerators:
    val values: IndexedSeq[Basic] = findValues
  }
  println("Basic.values = " + Basic.values)

  // Subsets of enumerators:
  sealed trait Subsets extends EnumEntry
  object Subsets extends Enum[Subsets] {

    trait Primary
    case object Red extends Subsets with Primary
    case object Blue extends Subsets with Primary

    trait Secondary extends Subsets
    case object Purple extends Secondary

    case object Gray extends Subsets

    val values = findValues
  }
  println("Subsets.values = " + Subsets.values)


  // Nested (any use?)--findValues finds (some) nested enumerators:
  sealed trait Nested extends EnumEntry
  object Nested extends Enum[Nested] {
    case object TypicalEnumerator extends Nested
    object Nested {
      case object NestedEnumerator extends Nested
    }
    val values = findValues
  }
  println("Nested.values = " + Nested.values)


  sealed trait Abnormal1 extends EnumEntry
  object Abnormal1 extends Enum[Abnormal1] {
    val values: IndexedSeq[Abnormal1] = findValues

    case object One extends Abnormal1
    trait Subgroup extends Abnormal1
    case object Two extends Subgroup

  }
  case object TwoOne extends Abnormal1  // not listed
  // findValues apparently lists only items in containing object, any only those
  //   of right type
  println("Abnormal1.values = " + Abnormal1.values)







}
