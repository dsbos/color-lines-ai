package com.us.dsb.explore.movethis


object ScoringState {
  def apply[TIME](nextAvailableTime: TIME): ScoringState[TIME] = {
    ScoringState(nextAvailableTime, 0, 0, 0)
  }
}

/**
  * Minimum state need to score subsequent orders.  (Does not include scheduled
  * order(s).)
  *
  * @param  nextAvailableTime  next time drone is available to start delivery
  * @param  promCount  number of orders expecting promoter-level satisfaction
  * @param  neutCount  number of orders expecting neutral satisfaction
  * @param  detrCount  number of orders expecting detractor-level satisfaction
  * @tparam  TIME
  */
case class ScoringState[TIME](nextAvailableTime: TIME,
                              promCount: Int,
                              neutCount: Int,
                              detrCount: Int) {
  private def npsOf(promCount: Int, neutCount: Int, detrCount: Int): Float = {
    val total = promCount + neutCount + detrCount
    val promPct = 100f * promCount / total  //???? address "/ 0.0f (-> NaN)"
    val detrPct = 100f * detrCount / total
    val npsPct = promPct - detrPct
    npsPct
  }

  /**
    * Expected net-promoter score.
    */
  val npsPct = npsOf(promCount, neutCount,detrCount)

  def withNextAvailableTime(newValue: TIME): ScoringState[TIME] = {
    copy(nextAvailableTime = newValue)
  }
  def withIncrementedSatCat(satCat: SatisfactionCategory): ScoringState[TIME] = {
    satCat match {
      case Promoter  => copy(promCount = promCount + 1)
      case Neutral   => copy(neutCount = neutCount + 1)
      case Detractor => copy(detrCount = detrCount + 1)
    }
  }

  /**
    * Calculates the maximum NPS that is possible from this state with a given
    * number of additional orders.
    */
  def maxPossibleNps(additional: Int): Float = {
    val maxPossiblePromCount  = promCount + additional
    val minPossibleDetrCount = detrCount
    val maxNps = npsOf(maxPossiblePromCount, neutCount, minPossibleDetrCount)
    maxNps
  }

  /**
    * Calculates the minimum NPS that is possible from this state with a given
    * number of additional orders.
    */

  def minPossibleNps(additional: Int): Float = {
    val minPossiblePromCount  = promCount
    val maxPossibleDetrCount  = detrCount + additional
    val minNps = npsOf(minPossiblePromCount, neutCount, maxPossibleDetrCount)
    minNps
  }
}

