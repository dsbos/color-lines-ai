package com.us.dsb.explore.movethis

import java.io.OutputStreamWriter
import java.time.LocalTime
import scala.io.Source


/**
  * Quickly thrown-together main program for reading/parsing an order input
  * file and formatting/writing a schedule output file.  (No error explicit
  * error checking, inconsistent Scala vs. Java I/O, etc.)
  *
  * Program argument number one is (relative or absolute) pathname to input
  * file.
  */
object DroneScheduler extends App {

  val ordersIn: List[Order] = {
    val filePathname = args(0)
    val orderInput = Source.fromFile(filePathname, "US-ASCII")
    orderInput.getLines().map(Order.decode).toList
  }

  // NOTE:  I did not get to mimicking the streaming nature of the scheduling
  // (that is, the fact that at a given time, you won't know the orders that
  // haven't come in by that time, so you can do scheduling only with what you
  // know at the moment).

  val schedulingResults = OrdersScheduler.scheduleAllOrders(ordersIn)


  locally {
    val outputFilePathname = java.io.File.createTempFile("DroneSchedule_", ".dat")

    val os = new java.io.FileOutputStream(outputFilePathname.toString)
    val ow = new OutputStreamWriter(os, "UTF-8")

    def formatTimeHACK(t: LocalTime): String = {
      f"${t.getHour}%02d:${t.getMinute}%02d:${t.getSecond}%02d"
    }

    val outputLines = {
      schedulingResults.schedule.map(step => {
        s"${step.orderId} ${formatTimeHACK(step.departureTime)}"
      }) :+
      s"NPS ${schedulingResults.npsPct.toInt}"
    }
    val outputContents = outputLines.mkString("\n")

    println(s"outputContents = '$outputContents'")

    ow.write(outputContents)
    ow.close

    println(outputFilePathname)
  }
}
