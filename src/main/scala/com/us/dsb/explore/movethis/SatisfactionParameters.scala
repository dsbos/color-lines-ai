package com.us.dsb.explore.movethis


object SatisfactionParameters {
  final val P_VS_N_BOUNDARY_HOURSxx = 1.5  // ??: unclear where between 1 and 2
  final val N_VS_D_BOUNDARY_HOURSxx = 3.5  // ??: unclear where between 3 and 4


  final val P_VS_N_BOUNDARY_SECSxx = (P_VS_N_BOUNDARY_HOURSxx * 60 * 60).toInt  // s/hr
  final val N_VS_D_BOUNDARY_SECSxx = (N_VS_D_BOUNDARY_HOURSxx * 60 * 60).toInt

}
