package com.us.dsb.explore.lang

object SingleAbstractMethodValueClassBug extends App {

  case class PlainClass(value: String)

  case class ValueClass(value: String) extends AnyVal

  case class SourceType1(value: String) extends AnyVal

  case class SourceType2(value: String)


  trait ToPlainClassMapper[ID] {
    def singleMethod(sourceValue: ID): PlainClass
  }

  trait ToValueClassMapper[ID] {
    def singleMethod(sourceValue: ID): ValueClass
  }

  val toPlainClassFromSource1: ToPlainClassMapper[SourceType1] = {
    new ToPlainClassMapper[SourceType1] {
      override def singleMethod(sourceValue: SourceType1): PlainClass = {
        PlainClass(sourceValue.value)
      }
    }
  }
  val toPlainClassFromSource2: ToPlainClassMapper[SourceType1] =
    sourceValue => PlainClass(sourceValue.value)

  val toValueClassFromSource1: ToValueClassMapper[SourceType1] = {
    new ToValueClassMapper[SourceType1] {
      override def singleMethod(sourceValue: SourceType1): ValueClass = {
        ValueClass(sourceValue.value)
      }
    }
  }
  val toValueClassFromSource2: ToValueClassMapper[SourceType1] =
    sourceValue => ValueClass(sourceValue.value)


  println("1. To plain class, explicit method syntax (works):")
  toPlainClassFromSource1.singleMethod(SourceType1("id"))
  println(".")

  println("2. To plain class, single abstract method syntax (works):")
  toPlainClassFromSource2.singleMethod(SourceType1("id"))
  println(".")

  println("3. To value class, explicit method syntax (works):")
  toValueClassFromSource1.singleMethod(SourceType1("id"))
  println(".")

  println("4. To value class, single abstract method syntax (fails):")
  // Fails with "java.lang.ClassCastException:
  //   com.us.dsb.explore.lang.SingleAbstractMethodValueClassBug$ValueClass
  //   cannot be cast to java.lang.String" at toValueClassFromSource2's lambda:
  toValueClassFromSource2.singleMethod(SourceType1("id"))
  println(".")
}
