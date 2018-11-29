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
    case class OrderAndHighestNps(nextOrder: Option[Order],  //??? first order, or next (after ordersSoFar)?
                                  ordersSoFar: List[Order],  //????
                                  PROBE:Int,scoringSoFar: List[ScoringState[LocalTime]],
                                  npsPct: Float)  //????? add ... to return schedule to caller

    var bestCompletedNpsSoFarQuickHack = -101f  //??? is this needed now that there's bestMinNpsSoFarHack?
    var bestMinNpsSoFarHack = -102f
    println(s"pickNextOrderToDeliver(...) . 0: bestCompletedNpsSoFarQuickHack := " + bestCompletedNpsSoFarQuickHack)
    println(s"pickNextOrderToDeliver(...) . 0: bestMinNpsSoFarHack := " + bestMinNpsSoFarHack)
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
    def pickNextOrder(ordersSoFar: List[Order],
                      scoringStateSoFar: ScoringState[LocalTime],
                      scoringChainSoFar1: List[ScoringState[LocalTime]],   // turn into schedule (with more data)?
                      remainingOrders: List[Order]
                     ): OrderAndHighestNps = {
      callCount += 1
      val depth = orders.size - remainingOrders.size
      val indentation = (1 to depth).map(_ => "  ").mkString
      if (true) {
        println(s"$indentation- pickNextOrder(...) . 1")
        println(s"$indentation- pickNextOrder(...) . 1: ordersSoFar       = " +
                ordersSoFar.map(_.id))
        println(s"$indentation- pickNextOrder(...) . 1: scoringStateSoFar = " +
                scoringStateSoFar)
        println(s"$indentation- pickNextOrder(...) . 1: scoringChainSoFar1 = " +
                scoringChainSoFar1)
        println(s"$indentation- pickNextOrder(...) . 1: remainingOrders = " +
                remainingOrders.map(_.id).mkString("[", ", ", "]"))
        println("")
      }

      val temp =
        if (! scoringStateSoFar.nextAvailableTime.isBefore(DroneParameters.WINDOW_END)) {
          ???
          OrderAndHighestNps(None, Nil, 123,scoringChainSoFar1, scoringStateSoFar.npsPct)
        }
        else {
          remainingOrders match {
            case Nil =>
              //println(s"$indentation- pickNextOrder(...) . Nil: scoringSoFar.npsPct = " + scoringSoFar.npsPct)
              if (scoringStateSoFar.npsPct > bestCompletedNpsSoFarQuickHack) {
                bestCompletedNpsSoFarQuickHack = scoringStateSoFar.npsPct
                println(s"$indentation- pickNextOrder(...) . Nil: bestCompletedNpsSoFarQuickHack := " +
                        bestCompletedNpsSoFarQuickHack + " (for " + ordersSoFar.map(_.id).mkString(", ") + ")")
                print("")
              }
              if (scoringStateSoFar.minPossibleNps(0) > bestMinNpsSoFarHack) {  //??? with zero, is same as just npsPct()
                bestMinNpsSoFarHack = scoringStateSoFar.minPossibleNps(0)
                println(s"$indentation- pickNextOrder(...) . Nil: bestMinNpsSoFarHack := " +
                        bestMinNpsSoFarHack + " (for " + ordersSoFar.map(_.id).mkString(", "))
                print("")
              }
              OrderAndHighestNps(None,
                                 remainingOrders /* Nil */,
                                 321,scoringChainSoFar1,
                                 scoringStateSoFar.npsPct)
            case list =>
              //println(s"$indentation- pickNextOrder(...) . list, size = ${list.size}")
              var bestChildResults = OrderAndHighestNps(None, ordersSoFar, 123,scoringChainSoFar1, -103.0f)

              //???? sort by heuristic here?
              //???? or maybe get with-child scores first, and then order before recursing?

              val childIncrScorePairs =
                list.map(childOrder => {
                  (childOrder, OrderingScorer.calcScoringStateWithOrder(scoringStateSoFar, childOrder))
                })
              //???? maybe update bestMinNpsSoFarHack here?

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

              for (childAndScoring <- sortedChildIncrScores) {
                println(s"$indentation- pickNextOrder(...) . list . ${childAndScoring._1.id}")
                val child = childAndScoring._1
                val remainingChildren = list.filterNot(_.id == child.id)
                val scoreWithChild = childAndScoring._2

                print("")

                if (scoreWithChild.minPossibleNps(remainingChildren.size) > bestMinNpsSoFarHack) {
                  bestMinNpsSoFarHack = scoreWithChild.minPossibleNps(remainingChildren.size)
                  println(s"$indentation- pickNextOrder(...) . list . ${child.id}: bestMinNpsSoFarHack := " +
                          bestMinNpsSoFarHack + " (for " + ordersSoFar.map(_.id).mkString("[", ", ", "]") + " + " + child)
                  print("")
                }

                if (true && scoreWithChild.maxPossibleNps(remainingChildren.size) < bestMinNpsSoFarHack) {
                  pruningCount += 1
                  //println(s"$indentation- pickNextOrder(...) . list . ${subHead.id}: pruned ... rest.size = ${rest.size}")
                  print("")
                }
                else {
                  print("")

                  //????? prune out this call if current best case is worse than bestMinNpsSoFarHack
                  val OrderAndHighestNps(_, xWHICHordersSoFar, _,scoringChainSoFar2, headBestNpsPct) =
                    pickNextOrder(ordersSoFar :+ child,
                                  scoreWithChild,
                                  scoringChainSoFar1 :+ scoreWithChild,  //???? add here on way down, or below on way up?
                                  remainingChildren)
                  if (headBestNpsPct > bestChildResults.npsPct) {
                    val ordersSoFarWithChild = xWHICHordersSoFar :+ child
                    bestChildResults =
                        OrderAndHighestNps(Some(child),
                                           ordersSoFarWithChild,
                                           123,scoringChainSoFar2 :+ childAndScoring._2,
                                           headBestNpsPct)
                    if (false) {
                      println(s"$indentation- pickNextOrder(...) . list . ${child.id}:" +
                              s" bestChildResults: ${bestChildResults.nextOrder.get.id}," +
                              s" npsPct = " + bestChildResults.npsPct)
                      print("")
                    }
                  }
                }
              }
              bestChildResults
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
    val firstOrderPickingResults: OrderAndHighestNps =
      pickNextOrder(Nil,
                    ScoringState(availabilityTime),
                    Nil,
                    if (false) sortedOrders else orders)
    println("firstOrderPickingResults = " + firstOrderPickingResults)
    println("firstOrderPickingResults.ordersSoFar:")
    firstOrderPickingResults.ordersSoFar.foreach(order => {
      println("  - " + order)
    })
    println("firstOrderPickingResults.scoringSoFar:")
    firstOrderPickingResults.scoringSoFar.foreach(scoring => {
      println("  - " + scoring)
    })
    val temp =
      firstOrderPickingResults.scoringSoFar.zip(firstOrderPickingResults.ordersSoFar)
    println("xxx:")
    temp.foreach(temp2 => {
      println("  - " + temp2)
    })

    println("firstOrderPickingResults.npsPct = " + firstOrderPickingResults.npsPct)
    println("pruningCount = " + pruningCount)
    println("callCount = " + callCount)

    firstOrderPickingResults.nextOrder  // ???? first order to schedule (only "next" assuming called in loop)
  }


}
