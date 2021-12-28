package com.us.dsb.explore.algs.walmart

import java.time.LocalTime
import java.time.temporal.ChronoUnit


object OrderingScorer {

  // (For dealing with choice of using LocalTime for times (because the
  // problem statement seemed to deal with one day in isolation).)
  private def calcLaterTimeToday(baseTime: LocalTime,
                                 intervalSecs: Long): Option[LocalTime] = {
    assert(0 <= intervalSecs)
    val rawSum = baseTime.plusSeconds(intervalSecs)
    if (rawSum.isBefore(baseTime)) {
      // Overflowed LocalTime, so past midnight, so not a time in same day.
      None
    }
    else {
      Some(rawSum)
    }
  }

  /** ... data that's needed to go from one SchedulingStep to next  */
  private case class OrderScoringIncrement(startingAvailableTime: LocalTime,
                                           orderId: String,
                                           departureTime: LocalTime,  // TODO: Option (like deliveryTime)
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
    scoringStateOut
  }


  def scoreOrdering(ordering: List[Order]): Float = {
    var scoringState = ScheduleStep[LocalTime](DroneParameters.WINDOW_START)
    for (order <- ordering) {
      scoringState = calcScoringStateWithOrder(scoringState, order)
    }
    scoringState.npsPct
  }
}
