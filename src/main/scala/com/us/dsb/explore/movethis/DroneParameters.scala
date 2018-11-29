package com.us.dsb.explore.movethis

import java.time.LocalTime


object DroneParameters {
  val SPEED_UNITS_PER_MIN = 1d
  val WINDOW_START: LocalTime = LocalTime.parse("06:00:00")
  val WINDOW_END:   LocalTime = LocalTime.parse("22:00:00")
}
