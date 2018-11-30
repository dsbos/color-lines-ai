package com.us.dsb.explore.movethis

import java.time.LocalTime
import java.time.temporal.ChronoUnit


object DummyDroneScheduler extends App {

  System.err.println("args = " + args)
  this.args

  val orders1 =
    List[Order](
      /*
      Order("XX0001",  0, 100, LocalTime.parse("05:00:00")),
      */
      Order("XX0002", LocalTime.parse("05:01:00"),  0,   5),
      Order("XX0003", LocalTime.parse("05:02:00"),  0,  50),
      Order("XX0004", LocalTime.parse("05:59:00"),  0,  10),
      /*
      Order("XX0005", LocalTime.parse("16:00:00"),  0,  71),

      Order("XX0007", LocalTime.parse("16:00:00"), 70,  70),

      */
      Order("XX0008", LocalTime.parse("16:00:00"),  0,  15),
      //Order("XX0010", LocalTime.parse("16:00:00"), 0,  99),
      //Order("XX0021", LocalTime.parse("16:00:00"), 0,  64),
      Order("XX0022", LocalTime.parse("16:00:00"),  0,  32),

      Order("XX0023", LocalTime.parse("16:00:00"),  0,  16),
      Order("XX0024", LocalTime.parse("16:00:00"),  0,   8)
    ).take(
      // 0
      5
    )


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
