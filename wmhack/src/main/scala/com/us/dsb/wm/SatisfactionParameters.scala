package com.us.dsb.wm

object SatisfactionParameters {
  final val P_VS_N_BOUNDARY_HOURS = 1.5  // ??: unclear where between 1 and 2
  final val N_VS_D_BOUNDARY_HOURS = 3.5  // ??: unclear where between 3 and 4

  final val P_VS_N_BOUNDARY_SECS = (P_VS_N_BOUNDARY_HOURS * 60 * 60).toInt  // s/hr
  final val N_VS_D_BOUNDARY_SECS = (N_VS_D_BOUNDARY_HOURS * 60 * 60).toInt

}
