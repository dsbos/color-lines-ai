package com.us.dsb.colorlines.expl.nn

object ActivationFunctions {

  /**
   * ...
   * @param x0
   * x values of function's midpoint
   * @param L
   * supremum of the function
   * @param k
   * logistics grawth rate
   * @param x
   * @return
   */
  def logisticFunction(x0: Double, L: Double, k: Double, x: Double): Double = {
    1 / (1 + math.exp(-(x - x0)))
  }

  /** ... zero x0, L, and k */
  def standardLogisticFunction(x: Double): Double = {
    logisticFunction(x0 = 0, L = 1, k = 1, x)
  }

}
