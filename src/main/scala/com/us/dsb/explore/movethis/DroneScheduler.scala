package com.us.dsb.explore.movethis

import java.time.LocalTime
import java.time.temporal.ChronoUnit


object DroneScheduler extends App {

  val orders1 =
    List[Order](
      /*
      Order("XX0001",  0, 100, LocalTime.parse("05:00:00")),
      */
      Order("XX0002",  0,   5, LocalTime.parse("05:01:00")),
      Order("XX0003",  0,  50, LocalTime.parse("05:02:00")),
      Order("XX0004",  0,  10, LocalTime.parse("05:59:00")),
      /*
      Order("XX0005",  0,  71, LocalTime.parse("16:00:00")),

      Order("XX0007", 70,  70, LocalTime.parse("16:00:00")),

      */
      Order("XX0008",  0,  15, LocalTime.parse("16:00:00")),
      //Order("XX0010", 99,  99, LocalTime.parse("16:00:00")),
      //Order("XX0021", 64,  64, LocalTime.parse("16:00:00")),
      Order("XX0022", 32,  32, LocalTime.parse("16:00:00")),

      Order("XX0023", 16,  16, LocalTime.parse("16:00:00")),
      Order("XX0024",  8,   8, LocalTime.parse("16:00:00"))
    ).take(10)


  if (false) for (order <- orders1) {
    import Math._
    val orderDistance = sqrt(pow(order.northing, 2) + pow(order.easting, 2))
    val deliveryTimeMins = orderDistance / DroneParameters.SPEED_UNITS_PER_MIN

    val pnThreshold = order.time.plusSeconds(SatisfactionParameters.P_VS_N_BOUNDARY_SECSxx)
    val ndThreshold = order.time.plusSeconds(SatisfactionParameters.N_VS_D_BOUNDARY_SECSxx)


    println()
    println("- order.id       = " + order.id)
    println("- order.northing = " + order.northing)
    println("- order.easting  = " + order.easting)
    println("- order.time     = " + order.time)
    println("- order.distance = " + order.distance)
    println("- deliveryTimeMins = " + deliveryTimeMins)
    println("- pnThreshold = " + pnThreshold)
    println("- ndThreshold = " + ndThreshold)
    //println("- xxx = " + xxx)

  }


  val next2 = NextOrderPicker2.pickNextOrderToDeliver(DroneParameters.WINDOW_START,
                                                     orders1)
  println("from NOP2.pickNextOrderToDeliver: " + next2)

  if (false) {
    val next1 = NextOrderPicker1.pickNextOrderToDeliver(DroneParameters.WINDOW_START,
                                                        orders1)
    println("from NOP1.pickNextOrderToDeliver: " + next1)
  }

  if (false) {
    val scorings: List[(List[Order], Double)] = {
      for (ordering <- orders1.permutations) yield {
        //println("ordering: " + ordering.map(_.id).mkString(", "))
        val score = OrderingScorer.scoreOrdering(ordering)
        println("ordering: " + ordering.map(_.id).mkString(", ") + ": " + score)
        (ordering, score)
      }
    }.toList
    //??println("scorings = " + scorings)
    val maxPermScore = scorings.maxBy(_._2)

    println("maxPermScore = " + maxPermScore)
  }





}
