package com.us.dsb.explore.movethis

import java.time.LocalTime
import java.time.temporal.ChronoUnit


object NextOrderPicker2 {





  //?????? ADD PRUNING
  // - maybe recognize when current head can't be delivered
  // - maybe recognize when nothing further can be delivered
  // - keep some kind of best score so far
  //   - maybe use something ~linear, not NPS
  //   - maybe use remaining depth (generally, or at certain pruning points):
  //     - to know maximum addition to Prom and Neut?
  //     - to know maximum dilution of Prom and Nuet by addition to Detr?


  /*
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


  def calcSecondsToNdBoundary(baseTime: LocalTime, order: Order): Long = {
    println()
    println("order.id              = " + order.id)
    // 1. trying by time to n/d threshold
    println("order.time            = " + order.time)

    val ndWindowLengthSecs = SatisfactionParameters.N_VS_D_BOUNDARY_SECSxx
    //println("ndWindowLengthSecs    = " + ndWindowLengthSecs)

    val ndDropoffBoundaryTime = order.time.plusSeconds(ndWindowLengthSecs)
    //println("ndDropoffBoundaryTime = " + ndDropoffBoundaryTime)

    val oneWayTripTimeSecs = (order.distance / DroneParameters.SPEED_UNITS_PER_MIN * 60).toInt
    //println("oneWayTripTimeSecs    = " + oneWayTripTimeSecs)

    val ndStartBoundaryTime =
      ndDropoffBoundaryTime.minusSeconds(oneWayTripTimeSecs)
    //println("ndStartBoundaryTime   = " + ndStartBoundaryTime)

    //println("baseTime              = " + baseTime)
    val secondsToNdBoundary = baseTime.until(ndStartBoundaryTime, ChronoUnit.SECONDS)
    println("secondsToNdBoundary   = " + secondsToNdBoundary)

    secondsToNdBoundary  //???

  }


  def pickNextOrderToDeliver(availabilityTime: LocalTime,
                             orders: List[Order]): Option[Order] = {
    //?? rename:
    case class OrderAndHighestNpsxx(nextOrder: Option[Order],
                                  ordering: List[Order],
                                  scoring: List[ScoringState[LocalTime]],
                                  npsPct: Float)  //????? add ... to return schedule to caller

    var bestCompletedNpsSoFarQuickHackxx = -101f
    var bestMinNpsSoFarHack = -102f
    println(s"pickNextOrderToDeliver(...) . 0: bestCompletedNpsSoFarQuickHack := " + bestCompletedNpsSoFarQuickHackxx)
    println(s"pickNextOrderToDeliver(...) . 0: bestMinNpsSoFarHack := " + bestMinNpsSoFarHack)
    var callCount = 0
    var pruningCount = 0

    def pickNextOrder(ordersSoFar: List[Order],
                      scoringIncr: ScoringState[LocalTime],   // ADDING list of incrs., for OrderAndHighestNpsxx.scoring
                      scoringSoFarxx: List[ScoringState[LocalTime]],   // ADDING list of incrs., for OrderAndHighestNpsxx.scoring
                      remainingOrders: List[Order]
                     ): OrderAndHighestNpsxx = {
      callCount += 1
      val depth = orders.size - remainingOrders.size
      val indentation = (1 to depth).map(_ => "  ").mkString
      if (true) {
        println(s"$indentation- pickNextOrder(...) . 1")
        println(s"$indentation- pickNextOrder(...) . 1: scoringIncr     = " +
                scoringIncr)
        println(s"$indentation- pickNextOrder(...) . 1: ordersSoFar     = " +
                ordersSoFar.map(_.id))
        println(s"$indentation- pickNextOrder(...) . 1: scoringSoFarxx  = " +
                scoringSoFarxx)
        println(s"$indentation- pickNextOrder(...) . 1: remainingOrders = " +
                remainingOrders.map(_.id).mkString("[", ", ", "]"))
        println("")
      }

      val temp =
        if (! scoringIncr.nextAvailableTime.isBefore(DroneParameters.WINDOW_END)) {
          ???
          OrderAndHighestNpsxx(None, Nil, scoringSoFarxx, scoringIncr.npsPct)
        }
        else {
          remainingOrders match {
            case Nil =>
              //println(s"$indentation- pickNextOrder(...) . Nil: scoringSoFar.npsPct = " + scoringSoFar.npsPct)
              if (scoringIncr.npsPct > bestCompletedNpsSoFarQuickHackxx) {
                bestCompletedNpsSoFarQuickHackxx = scoringIncr.npsPct
                println(s"$indentation- pickNextOrder(...) . Nil: bestCompletedNpsSoFarQuickHackxx := " +
                        bestCompletedNpsSoFarQuickHackxx + " (for " + ordersSoFar.map(_.id).mkString(", "))
                print("")
              }
              if (scoringIncr.minPossibleNps(0) > bestMinNpsSoFarHack) {
                bestMinNpsSoFarHack = scoringIncr.minPossibleNps(0)
                println(s"$indentation- pickNextOrder(...) . Nil: bestMinNpsSoFarHack := " +
                        bestMinNpsSoFarHack + " (for " + ordersSoFar.map(_.id).mkString(", "))
                print("")
              }
              OrderAndHighestNpsxx(None, Nil, scoringSoFarxx, scoringIncr.npsPct)
            case list =>
              //println(s"$indentation- pickNextOrder(...) . list, size = ${list.size}")
              var bestChildSoFar = OrderAndHighestNpsxx(None, ordersSoFar, scoringSoFarxx, -103.0f)

              //???? sort by heuristic here?
              //???? or maybe get with-child scores first, and then order before recursing?

              val childIncrScorePairs =
                list.map(subHead => {
                  (subHead, OrderingScorer.calcScoringStateWithOrder(scoringIncr, subHead))
                })

              val sortedChildIncrScores =
                childIncrScorePairs.sortBy(orderAndScoring => {
                  //calcSecondsToNdBoundary(availabilityTime, orderAndScoring._1)
                  - orderAndScoring._2.maxPossibleNps(childIncrScorePairs.size)
                  // orderAndScoring._2.maxPossibleNps(childIncrScores.size) - worsens, as expected
                  //0
                  //orderAndScoring._1.distance - helps little
                  //orderAndScoring._1.time
                  //orderAndScoring._1.id

                })


              for (subHeadIncr <- sortedChildIncrScores) {
                println(s"$indentation- pickNextOrder(...) . list . ${subHeadIncr._1.id}")
                val subHead = subHeadIncr._1
                val rest = list.filterNot(_.id == subHead.id)
                val scoreWithHead = subHeadIncr._2

                print("")

                if (scoreWithHead.minPossibleNps(rest.size) > bestMinNpsSoFarHack) {
                  bestMinNpsSoFarHack = scoreWithHead.minPossibleNps(rest.size)
                  println(s"$indentation- pickNextOrder(...) . list . ${subHead.id}: bestMinNpsSoFarHack := " +
                          bestMinNpsSoFarHack + " (for " + ordersSoFar.map(_.id).mkString("[", ", ", "]") + " + " + subHead)
                  print("")
                }

                if (true && scoreWithHead.maxPossibleNps(rest.size) < bestMinNpsSoFarHack) {
                  pruningCount += 1
                  //println(s"$indentation- pickNextOrder(...) . list . ${subHead.id}: pruned ... rest.size = ${rest.size}")
                  print("")
                }
                else {
                  print("")

                  //????? prune out this call if current best case is worse than bestMinNpsSoFarHack
                  val OrderAndHighestNpsxx(_, nameThis2,  scoring, headBestNpsPct) =
                    pickNextOrder(ordersSoFar :+ subHead, scoreWithHead, scoringSoFarxx :+ scoreWithHead, rest)
                  if (headBestNpsPct > bestChildSoFar.npsPct) {
                    val confirmThis = subHead :: nameThis2
                    bestChildSoFar = OrderAndHighestNpsxx(Some(subHead), confirmThis, scoringSoFarxx :+ subHeadIncr._2, headBestNpsPct)
                    if (false) {
                      println(s"$indentation- pickNextOrder(...) . list . ${subHead.id}:" +
                              s" bestChildSoFar: ${bestChildSoFar.nextOrder.get.id}, npsPct = " + bestChildSoFar.npsPct)
                      print("")
                    }
                  }
                }

              }
              bestChildSoFar
          }
        }
      if (false) {
        println(s"$indentation- pickNextOrder(...) . 9: temp = " + temp +
                "; - ordersSoFar = " + ordersSoFar.map(_.id))
      }
      temp
    }
    val sortedOrders = orders.sortBy(order => {
      calcSecondsToNdBoundary(availabilityTime, order)
      //??order.distance
    })
    val nameThis1 = pickNextOrder(Nil, ScoringState(availabilityTime), Nil, if (false) sortedOrders else orders)
    println("nameThis1 = " + nameThis1)
    nameThis1.ordering.foreach(order => {
      println("- " + order)
    })
    println("nameThis1.npsPct = " + nameThis1.npsPct)
    println("pruningCount = " + pruningCount)
    println("callCount = " + callCount)

    nameThis1.nextOrder
  }


}
