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
  private case class OrderScoringIncrement(newNextAvailableTime: LocalTime,
                                           satCat: SatisfactionCategory)


  private def calcOrderScoringIncrement(scoringStateIn: ScoringState[LocalTime],
                                        order: Order
                                       ): OrderScoringIncrement = {

    val startTime2 =
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


    val startTime1 = scoringStateIn.nextAvailableTime

    if (false) {
      println
      println(f"@$startTime2: try $Order ${order.id}" +
              f" (${order.northing}%2d N / ${order.easting}%2d E - ${order.distance}%5.2f away):")
    }

    val oneWayTimeSecs = (order.distance / MiscConstants.UNITS_PER_SEC).toLong

    val deliveryTimeIfToday = calcLaterTimeToday(startTime2, oneWayTimeSecs)
    val returnTimeIfToday =
      deliveryTimeIfToday.flatMap(calcLaterTimeToday(_, oneWayTimeSecs))

    val (newNextAvailableTime, satCat: SatisfactionCategory) =
      if (returnTimeIfToday.fold(true)(_.isAfter(DroneParameters.WINDOW_END))) {
        // Can't return by time window end today, so can't deliver order
        // today--so next-availability time stays same, and customer will be
        // detractor.
        if (false) println(s"- misses ${DroneParameters.WINDOW_END} 'curfew'")
        (startTime2, Detractor)
      }
      else {
        val deliveryLatencySecs =
          order.time.until(deliveryTimeIfToday.get, ChronoUnit.SECONDS)       //??? FIX .get
        (returnTimeIfToday.get, SatisfactionCategory.categorizeLatency(deliveryLatencySecs))        //??? FIX .get
      }

    if (false) {
      println(f"@$startTime2: try $Order ${order.id}" +
              f" (${order.northing}%2d N / ${order.easting}%2d E - ${order.distance}%5.2f away):  $newNextAvailableTime, $satCat")
    }


    //println(s"- satCat = $satCat")
    OrderScoringIncrement(newNextAvailableTime, satCat)
  }

  def calcScoringStateWithOrder(scoringStateIn: ScoringState[LocalTime],
                                order: Order
                               ): ScoringState[LocalTime] = {
    val increment = calcOrderScoringIncrement(scoringStateIn, order)
    val scoringStateOut =
        scoringStateIn
            .withNextAvailableTime(increment.newNextAvailableTime)
            .withIncrementedSatCat(increment.satCat)
    //println("- scoringStateOut = " + scoringStateOut)
    scoringStateOut
  }


  def scoreOrdering(ordering: List[Order]): Double = {
    var scoringState = ScoringState[LocalTime](DroneParameters.WINDOW_START)
    for (order <- ordering) {
      scoringState = calcScoringStateWithOrder(scoringState, order)
    }
    val npsPct = scoringState.npsPct
    //println("- npsPct = " + npsPct)
    npsPct
  }
}
