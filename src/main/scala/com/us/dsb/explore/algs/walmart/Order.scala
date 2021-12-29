package com.us.dsb.explore.algs.walmart

import java.time.LocalTime
import java.util.regex.Pattern


case class Order(id: String, time: LocalTime, northing: Int, easting: Int) {
  import Math._
  val distance = sqrt(pow(northing, 2) + pow(easting, 2))
}


object Order {
  // Format: like "WM1234 N1E10 05:40:59"
  private val LOCATION_REGEXP = Pattern.compile("([NS])([0-9]+)([EW])([0-9]+)")

  // Quick hack (including: no explicit error checking/reporting):
  def decode(orderLine: String): Order = {
    // TODO:  Check out using Scala regular expression matching in Scala pattern
    // matches.

    val Array(orderId, locStr, timeStr) = orderLine.split(" ") : @unchecked
    val matchResult = LOCATION_REGEXP.matcher(locStr)

    matchResult.matches()  // need to call to avoid "No match found" for .group(...)
    val ns    = matchResult.group(1)
    val nsVal = matchResult.group(2)
    val ew    = matchResult.group(3)
    val ewStr = matchResult.group(4)

    val absNorthing = nsVal.toInt
    val northing =
      ns match {
        case "N" => absNorthing
        case "S" => -absNorthing
      }
    val absEasting = ewStr.toInt
    val easting =
      ew match {
        case "E" => absEasting
        case "W" => -absEasting
      }

    val orderTime = LocalTime.parse(timeStr)
    Order(orderId, orderTime, northing, easting)
  }
}