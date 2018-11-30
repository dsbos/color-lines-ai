package com.us.dsb.explore.movethis


import java.time.LocalTime
import java.time.temporal.ChronoUnit


object OrderingScorer {


  // (For dealing with having used LocalTime for times.)
  private def calcLaterTimeToday(baseTime: LocalTime,
                                 intervalSecs: Long): Option[LocalTime] = {
    assert(0 <= intervalSecs)
    val rawSum = baseTime.plusSeconds(intervalSecs)
    if (rawSum.isBefore(baseTime)) {
      // Overflowed LocalTime, so past midnight, so nonviable  //?? move
      None
    }
    else {
      Some(rawSum)
    }
  }

  /** ... data's needed to go from one ScoringState to next  */
  private case class OrderScoringIncrement(startingAvailableTime: LocalTime,
                                           orderId: String,
                                           departureTime: LocalTime,  //??? optional (like deliveryTimexx4)
                                           deliveryTime: Option[LocalTime],
                                           satCat: SatisfactionCategory,
                                           newNextAvailableTime: LocalTime)


  private def calcOrderScoringIncrement(scoringStateIn: ScheduleStep[LocalTime],
                                        order: Order
                                       ): OrderScoringIncrement = {

    val startTime =
      if (order.time.isAfter(scoringStateIn.nextAvailableTime)) {
        // Order time is after time drone became available--can't schedule
        // before order was received, so delay start to order time.
        // (Note:  This case makes sense only in a retrospective calculation,
        // not in the normal streaming (real-time) case.
        order.time
      }
      else {
        scoringStateIn.nextAvailableTime
      }

    if (false) {
      println()
      println(f"@$startTime: try $Order ${order.id}" +
              f" (${order.northing}%2d N / ${order.easting}%2d E - ${order.distance}%5.2f away):")
    }

    val oneWayTimeSecs = (order.distance / MiscConstants.SPEED_UNITS_PER_SEC).toInt

    val deliveryTimeIfToday = calcLaterTimeToday(startTime, oneWayTimeSecs)
    val returnTimeIfToday =
      deliveryTimeIfToday.flatMap(calcLaterTimeToday(_, oneWayTimeSecs))

    val (newNextAvailableTime, satCat: SatisfactionCategory, deliveryTimeIfDelivered) =
      if (returnTimeIfToday.fold(true)(_.isAfter(DroneParameters.WINDOW_END))) {
        // Return time would be after window end today (including overflowing
        // flowed LocalTime and therefore today), so can't deliver order
        // today--so next-availability time stays same (for some other order),
        // and customer will be detractor.
        if (false) println(s"- misses ${DroneParameters.WINDOW_END} 'curfew'")
        (startTime, Detractor, None)
      }
      else {
        //?? TODO:  Try to rework to elimimate .get calls (without duplicating
        //  big comment just above).
        val deliveryLatencySecs =
          order.time.until(deliveryTimeIfToday.get, ChronoUnit.SECONDS)
        (returnTimeIfToday.get,
            SatisfactionCategory.categorizeLatency(deliveryLatencySecs),
            deliveryTimeIfToday)
      }

    if (false) {
      println(f"@$startTime: try $Order ${order.id}" +
              f" (${order.northing}%2d N / ${order.easting}%2d E" +
              f" - ${order.distance}%5.2f away):  $newNextAvailableTime, $satCat")
    }

    OrderScoringIncrement(scoringStateIn.nextAvailableTime,
                          order.id,
                          startTime,
                          deliveryTimeIfDelivered,
                          satCat,
                          newNextAvailableTime)
  }

  def calcScoringStateWithOrder(scoringStateIn: ScheduleStep[LocalTime],
                                order: Order
                               ): ScheduleStep[LocalTime] = {
    val increment = calcOrderScoringIncrement(scoringStateIn, order)
    val scoringStateOut =
        scoringStateIn
            .withStartingAvailableTime(increment.startingAvailableTime)
            .withOrderId(increment.orderId)
            .withDepartureTime(increment.departureTime)
            .withDeliveryTime(increment.deliveryTime)
            .withIncrementedSatCat(increment.satCat)
            .withNextAvailableTime(increment.newNextAvailableTime)
    //println("- scoringStateOut = " + scoringStateOut)
    scoringStateOut
  }


  def scoreOrdering(ordering: List[Order]): Double = {
    var scoringState = ScheduleStep[LocalTime](DroneParameters.WINDOW_START,
                                               DroneParameters.WINDOW_START,
                                               DroneParameters.WINDOW_START,
                                               DroneParameters.WINDOW_START)  //????? maybe new constructor?
    for (order <- ordering) {
      scoringState = calcScoringStateWithOrder(scoringState, order)
    }
    val npsPct = scoringState.npsPctxx
    //println("- npsPct = " + npsPct)
    npsPct
  }
}
