package com.us.dsb.explore.movethis

import java.time.LocalTime


object NextOrderPicker1 {


  //?????? ADD PRUNING
  // - maybe recognize when current head can't be delivered
  // - maybe recognize when nothing further can be delivered
  // - keep some kind of best score so far
  //   - maybe use something ~linear, not NPS
  //   - maybe use remaining depth (generally, or at certain pruning points):
  //     - to know maximum addition to Prom and Neut?
  //     - to know maximum dilution of Prom and Nuet by addition to Detr?

  def pickNextOrderToDeliver(availabilityTime: LocalTime,
                             orders: List[Order]): Option[Order] = {

    //?? rename:
    case class OrderAndHighestNps(nextOrder: Option[Order],
                                  ordering: List[Order],
                                  npsPct: Float)

    def pickNextOrder(scoreSoFar: ScoringState[LocalTime],
                      ordersSoFar: List[Order],
                      remainingOrders: List[Order]
                     ): OrderAndHighestNps = {
      val depth = orders.size - remainingOrders.size
      val indentation = (1 to depth).map(_ => "  ").mkString
      if (false) {
        println(s"$indentation- pickNextOrder(...) . 1")
        println(s"$indentation- pickNextOrder(...) . 1: ordersSoFar = " +
                ordersSoFar.map(_.id))
      }

      val temp =
        if (! scoreSoFar.nextAvailableTime.isBefore(DroneParameters.WINDOW_END)) {
          ???
          OrderAndHighestNps(None, Nil, scoreSoFar.npsPct)
        }
        else {
          remainingOrders match {
            case Nil =>
              //println(s"$indentation- pickNextOrder(...) . Nil")
              OrderAndHighestNps(None, Nil, scoreSoFar.npsPct)
            case list =>
              //println(s"$indentation- pickNextOrder(...) . list")
              var bestSoFar = OrderAndHighestNps(None, ordersSoFar, -101.0f)
              for (subHead <- list) {
                //println(s"$indentation- pickNextOrder(...) . list . ${subHead.id}")
                val rest = list.filterNot(_.id == subHead.id)
                val scoreWithHead =
                  OrderingScorer.calcScoringStateWithOrder(scoreSoFar, subHead)

                val OrderAndHighestNps(_, nameThis2, headBestNpsPct) =
                  pickNextOrder(scoreWithHead, ordersSoFar :+ subHead, rest)
                if (headBestNpsPct > bestSoFar.npsPct) {
                  val confirmThis = subHead :: nameThis2
                  bestSoFar = OrderAndHighestNps(Some(subHead), confirmThis, headBestNpsPct)
                }
              }
              bestSoFar
          }
        }
      println(s"$indentation- pickNextOrder(...) . 9: temp = " + temp +
              "; - ordersSoFar = " + ordersSoFar.map(_.id))
      temp
    }

    val nameThis1 = pickNextOrder(ScoringState(availabilityTime), Nil, orders)
    println("nameThis1 = " + nameThis1)
    nameThis1.nextOrder
  }


}
