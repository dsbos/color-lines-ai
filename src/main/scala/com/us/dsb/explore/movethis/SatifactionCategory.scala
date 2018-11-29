package com.us.dsb.explore.movethis


// ?? doc.
sealed trait SatisfactionCategory
case object Promoter extends SatisfactionCategory
case object Neutral extends SatisfactionCategory
case object Detractor extends SatisfactionCategory

object SatisfactionCategory {
  def categorizeLatency(deliveryLatencySecs: Long): SatisfactionCategory = {
    import SatisfactionParameters._
    deliveryLatencySecs match {
      case latency if latency < 0                    => ???  //??? report
      case latency if latency < P_VS_N_BOUNDARY_SECSxx => Promoter  // LT? LE?
      case latency if latency < N_VS_D_BOUNDARY_SECSxx => Neutral
      case _                                         => Detractor
    }
  }
}