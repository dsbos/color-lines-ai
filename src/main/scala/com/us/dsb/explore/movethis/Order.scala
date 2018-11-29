package com.us.dsb.explore.movethis

import java.time.LocalTime
import java.util.regex.Pattern


case class Order(id: String, northing: Int, easting: Int, time: LocalTime) {
  import Math._
  val distance = sqrt(pow(northing, 2) + pow(easting, 2))
}


object Order {

  private val LOCATION_REGEXP = Pattern.compile("([NS])([0-9]+)([EW])([0-9]+)")

  // Quick hack
  def decode(orderLine: String): Order = { //???? Option?

    // "WM1234 N1E10 05:40:59"
    val Array(orderId, locStr, timeStr) = orderLine.split(" ")

    val p = LOCATION_REGEXP


    //val t2 = t1.matcher(locStr)
    val t2 = LOCATION_REGEXP.matcher("N1E10")
    println("t2 = " + t2)
    t2.matches()  // need to call to avoid "No match found" for .group(...)

    val g1 = t2.group(1) ; println("g1 = " + g1)
    val g2 = t2.group(2) ; println("g2 = " + g2)
    val g3 = t2.group(3) ; println("g3 = " + g3)
    val g4 = t2.group(4) ; println("g4 = " + g4)
    val absNorthing = g2.toInt
    val northing =
      g1 match {
        case "N" => absNorthing
        case "S" => -absNorthing
      }
    val absEasting = g4.toInt
    val easting =
      g3 match {
        case "E" => absEasting
        case "W" => -absEasting
      }

    val orderTime = LocalTime.parse(timeStr)
    Order(orderId, northing, easting, orderTime)

    //??? check out Scala regular expression matching (for match/case)
  }

  def main(args: Array[String]): Unit = {
    val testIn = "WM1234 N1E10 05:40:59"
    val testOut = decode(testIn)
    println("testIn  = " + testIn)
    println("testOut = " + testOut)


  }

}