package com.us.dsb.explore.algs.walmart

import org.scalatest.funsuite.AnyFunSuite

import java.time.LocalTime


class OrderUnitTest extends AnyFunSuite {

  test("decode(...) should decode basic input") {
    val input = "WM1234 N1E10 00:00:00"
    val expected = Order("WM1234", LocalTime.of(0, 0, 0), +1, +10)
    val actual = Order.decode(input)
    assertResult(expected, s"- for '$input'")(actual)
  }

  import Order.decode
  test("decode(...) should decode south indication to negative") {
    val input = "WM1234 S1E10 05:40:59"
    assertResult(Order("WM1234", LocalTime.of(5, 40, 59), -1, +10),
                 s"- for '$input'")(decode(input))
  }

  test("decode(...) should decode west indication to negative") {
    val input = "WM1234 S1W10 23:59:59"
    assertResult(Order("WM1234", LocalTime.of(23, 59, 59), -1, -10),
                 s"- for '$input'")(decode(input))
  }
}
