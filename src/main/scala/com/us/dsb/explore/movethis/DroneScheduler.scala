package com.us.dsb.explore.movethis

import java.io.OutputStreamWriter
import java.time.LocalTime
import scala.io.Source


object DroneScheduler extends App {
  //?? HACK: Quickly thrown together main program for reading/parsing input
  // files and formatting/writing to output file.

  val ordersIn = {
    val filePathname = args(0) //?? errors
    val orderInput = Source.fromFile(filePathname, "US-ASCII")
    orderInput.getLines().map(Order.decode(_)).toList
  }
  print("ordersIn = " + ordersIn)

  val schedule =
    NextOrderPicker2.pickNextOrderToDeliver(DroneParameters.WINDOW_START,
                                            ordersIn)


  println("schedule = " + schedule)
  println("schedule.xx:")
  schedule.scoringSoFar.foreach(x =>
    println("- " + x)
  )
  println("schedule.npsPct = " + schedule.npsPct)

  locally {
    val outputFilePathname = java.io.File.createTempFile("DroneSchedule_", ".dat")
    println("outputFilePathname = " + outputFilePathname)


    val os = new java.io.FileOutputStream(outputFilePathname.toString)
    val ow = new OutputStreamWriter(os, "UTF-8")

    def formatTimeHACK(t: LocalTime): String = {
      f"${t.getHour}%02d:${t.getMinute}%02d:${t.getSecond}%02d"

    }

    val outputLines =
      schedule.scoringSoFar.map(x => s"${x.orderId} ${formatTimeHACK(x.departureTime)}") :+
      s"NPS ${schedule.npsPct.toInt}"
    val outputContents = outputLines.mkString("\n")

    println(s"outputContents = '$outputContents'")

    ow.write(outputContents)
    ow.close

    println(outputFilePathname)
  }
}
