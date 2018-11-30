package com.us.dsb.explore.movethis

import java.time.LocalTime
import java.time.temporal.ChronoUnit


object OrdersScheduler {


  /*
    Notes re traversal pruning:

    What are best and worst possible from given point?
    - best  is if all remaining are Prom:
      - (P + #) / (P + # + N + D) - D  / (P + # + N + D)
         =                 (P + # - D) / (P + # + N + D)

    - worst is if all remaining are Detr:
      - (P) / (P + N + D + #) - (D + #)  / (P + N + D + #)
         =                 (P - (D + #)) / (P + N + D + #)

    - (best minus worst narrows to zero as # goes to zero)

    - (best  goes towards  100% as # goes to infinity
    - (worst goes towards -100% as # goes to infinity)
    - ((both  go   towards   0% if neutCount goes to infinity))


    So if the best possible score for the current ordering prefix is worse than
    the worst possible score of all other processed ordering prefixes, drop
    the current partial ordering and prune its subtree.

    (That is, if current is worse than the saved "worst worst possible scoring",
    prune.)

    If an ordering prefix's worst possible score is better than the current
    saved "worst worst possible scoring", update to the better value.

   */

  /**
    * ... seconds until time for boundary between neutral vs. detractor ...
    */
  def calcSecondsToNdBoundary(baseTime: LocalTime, order: Order): Long = {
    val ndWindowLengthSecs = SatisfactionParameters.N_VS_D_BOUNDARY_SECS
    val ndDropoffBoundaryTime = order.time.plusSeconds(ndWindowLengthSecs)
    val oneWayTripTimeSecs = (order.distance /
                              DroneParameters.SPEED_UNITS_PER_MIN * 60).toInt
    val ndStartBoundaryTime =
      ndDropoffBoundaryTime.minusSeconds(oneWayTripTimeSecs)
    val secondsToNdBoundary = baseTime.until(ndStartBoundaryTime,
                                             ChronoUnit.SECONDS)
    secondsToNdBoundary
  }


  /**
    * Schedules given orders starting at given drone availability time.
    */
  def scheduleOrders(availabilityTime: LocalTime,
                     orders: List[Order]): SchedulingResults = {

    var bestCompletedNpsSoFarQuickHack = -101f  // TODO:  Purge if obsoleted by newer bestMinNpsSoFarHack.
    var bestMinNpsSoFarHack = -102f
    // For trying different heuristics:
    var callCount = 0
    var pruningCount = 0

    /**
      *
      * @param ordersSoFar
      * @param scoringStateSoFar
      * @param scoringChainSoFar
      * @param remainingOrders
      * @return
      */
    def scheduleRemainder(ordersSoFar: List[Order],
                          scoringStateSoFar: ScheduleStep[LocalTime],
                          scoringChainSoFar: List[ScheduleStep[LocalTime]],
                          remainingOrders: List[Order]
                         ): SchedulingResults = {
      callCount += 1

      val bestScheduleFromHere =
        if (! scoringStateSoFar.nextAvailableTime.isBefore(DroneParameters.WINDOW_END)) {
          SchedulingResults(None, Nil, scoringChainSoFar, scoringStateSoFar.npsPct)
        }
        else {
          remainingOrders match {
            case Nil =>
              if (scoringStateSoFar.npsPct > bestCompletedNpsSoFarQuickHack) {
                bestCompletedNpsSoFarQuickHack = scoringStateSoFar.npsPct
              }
              // TODO: Confirm and simplify: with zero, minPossibleNps(0) is
              // same as just npsPct():
              if (scoringStateSoFar.minPossibleNps(0) > bestMinNpsSoFarHack) {
                bestMinNpsSoFarHack = scoringStateSoFar.minPossibleNps(0)
              }
              SchedulingResults(None,
                                remainingOrders /* Nil */,
                                scoringChainSoFar,
                                scoringStateSoFar.npsPct)
            case list =>
              // Do one level of scheduling on each child (of traversal) before
              // proceeding with deeper traverals so that we can try processing
              // orders steps with higher possible NPS values before this with
              // lower values, so that maybe we can find the better values
              // sooner (after fewer other traversal stops) and then prune more
              // of the other branches that aren't as good.

              var bestChildResults =
                SchedulingResults(None, ordersSoFar, scoringChainSoFar, -103.0f)

              val childIncrScorePairs =
                list.map(childOrder => {
                  (childOrder,
                      OrderingScorer.calcScoringStateWithOrder(scoringStateSoFar, childOrder))
                })

              // Apply heuristic:  Prioritize branches with higher values for
              // their highest possible NPS (given remaining traversal depth):

              val sortedChildIncrScores =
                childIncrScorePairs.sortBy(orderAndScoring => {
                  - orderAndScoring._2.maxPossibleNps(childIncrScorePairs.size)
                })

              // Now traverse subtrees below children, starting with child with
              // highest possible NPS:
              for (childAndScoring <- sortedChildIncrScores) {
                val child = childAndScoring._1
                val scoreWithChild = childAndScoring._2
                val remainingChildren = list.filterNot(_.id == child.id)


                if (scoreWithChild.minPossibleNps(remainingChildren.size) > bestMinNpsSoFarHack) {
                  bestMinNpsSoFarHack = scoreWithChild.minPossibleNps(remainingChildren.size)
                }
                // Prune at any node whose maximum possible NPS is still less
                // than the minimum possible NPS seen so far for any other node.
                if (scoreWithChild.maxPossibleNps(remainingChildren.size) < bestMinNpsSoFarHack) {
                  pruningCount += 1
                }
                else {
                  val SchedulingResults(_, ordersSoFar2, scoringChainSoFar2, headBestNpsPct) =
                    scheduleRemainder(ordersSoFar :+ child,
                                      scoreWithChild,
                                      scoringChainSoFar :+ scoreWithChild,
                                      remainingChildren)
                  if (headBestNpsPct > bestChildResults.npsPct) {
                    // TODO:  Reverse List order (use efficient :: instead of :+).
                    bestChildResults =
                        SchedulingResults(Some(child),
                                           ordersSoFar2,
                                           scoringChainSoFar2,
                                           headBestNpsPct)
                  }
                }
              }
              bestChildResults
          }
        }
      bestScheduleFromHere
    }

    // NOTE:  Was trying traversing orders in order of descending time
    // urgency--least time left before hitting detractor-level delivery latency.
    // (Didn't seem to help much; disabled below.)
    val sortedOrders = orders.sortBy(order => {
      calcSecondsToNdBoundary(availabilityTime, order)
    })

    val schedulingResults: SchedulingResults =
      scheduleRemainder(Nil,
                        ScheduleStep(availabilityTime),
                        Nil,
                        if (false) sortedOrders else orders)
    schedulingResults
  }

  def scheduleAllOrders(orders: List[Order]): SchedulingResults = {
    scheduleOrders(DroneParameters.WINDOW_START, orders)
  }

}
