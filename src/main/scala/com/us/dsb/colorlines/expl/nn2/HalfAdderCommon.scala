package com.us.dsb.colorlines.expl.nn2

object HalfAdderCommon {

  /**
   * Training/evaluation cases.
   * 3 bits to add -> 2-bit sum (high bit, low bit).
   */
  val cases: List[((Byte, Byte, Byte), (Byte, Byte))] = {
    List(
      (0, 0, 0) -> (0, 0),
      (0, 0, 1) -> (0, 1),
      (0, 1, 0) -> (0, 1),
      (0, 1, 1) -> (1, 0),
      (1, 0, 0) -> (0, 1),
      (1, 0, 1) -> (1, 0),
      (1, 1, 0) -> (1, 0),
      (1, 1, 1) -> (1, 1),
      )
        .map { case ((a1, a2, a3), (c, s)) => 
          ((a1.toByte, a2.toByte, a3.toByte), (c.toByte, s.toByte))
        }
  }

  type ExecutionFunction = (Byte, Byte, Byte) => (Double, Double)
  //?????? Revisit names:
  // - case execution function
  // - network evaluation/fitness function
  def computeFitness(executeNetwork: ExecutionFunction): Double = {
    val fitness =
      cases.map { case ((a1, a2, a3), (c, s)) =>
        val nnOutput = executeNetwork(a1, a2, a3)
        val cError = nnOutput._1 - c
        val sError = nnOutput._2 - s
        val error = cError * cError + sError * sError
        val caseFitness = -error
        caseFitness
      }.sum
    //println(s"fitness = $fitness")
    fitness
  }

}
