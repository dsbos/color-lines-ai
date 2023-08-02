package com.us.dsb.colorlines.expl.nn2

import com.us.dsb.colorlines.expl.nn2.ActivationComputation.{
  ActivationFunction, computeNetworkOutputActivation}
import com.us.dsb.colorlines.expl.nn2.types.LowlevelTypes.{Activation, LayerActivations}
import com.us.dsb.colorlines.expl.nn2.types.NeuralNetworkReadView.NetworkConfig

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

  /**
   * Abstract execution function for half adder:  Input bit addends to approximate 2-bit output.
   */
  type ExecutionFunction = (Byte, Byte, Byte) => (Double, Double)
  //?????? Revisit names:
  // - case execution function
  // - network evaluation/fitness function

  /**
   * Fitness function for network for half adder, give abstract execution function.
   */
  def computeFunctionFitness(executeNetwork: ExecutionFunction): Double = {
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

  private val activationFunction: ActivationFunction =
    raw => Activation(ActivationFunctions.standardLogisticFunction(raw))

  //?????? Maybe wrap Byte in Bit opaque type to limit to 0 and 1:
  //?????? Possibly wrap Double in something reflecting approximated "bitness":
  def executeNetwork(nw: NetworkConfig,
                     inputs: (Byte, Byte, Byte)
                    ): (Double, Double) = {

    // ???? TODO:  Is there way to map tuple to collection?  Or have fixed length on
    //   collection?  (Tuple inputs and outputs are for fixed lengths for half-adder.)
    // ???? TODO:  Should LayerActivations have var-args ~constructor?
    val inputActivations = LayerActivations(IndexedSeq(Activation(inputs._1),
                                                       Activation(inputs._2),
                                                       Activation(inputs._3)))
    val outputActivations =
      computeNetworkOutputActivation(nw, activationFunction, inputActivations)
    (outputActivations.vector(0).raw,
        outputActivations.vector(1).raw)
  }

  // ?? TODO: _Possibly_ wrap Double in some opaque fitness type:
  /**
   * Fitness function for network for half adder, give neural network function.
   */
  def computeNetworkFitness(nw: NetworkConfig): Double =
    HalfAdderCommon.computeFunctionFitness(
      (a1: Byte, a2: Byte, a3: Byte) => executeNetwork(nw, (a1, a2, a3)))


  def run(makeInitialNetwork: NetworkConfig,
          makeCandidateNetwork: NetworkConfig => NetworkConfig): Unit = {
    val startMs = System.currentTimeMillis()
    var curr = makeInitialNetwork
    var currFitness = computeNetworkFitness(curr)
    val maxIterations = 100_000_000
    var iterations = 0
    while iterations < maxIterations do {
      iterations += 1
      if (1 == iterations % 1_000_000) {
        val pct = 100.0 * iterations / maxIterations
        println(f"@ $iterations (/$maxIterations, ${pct}%4.1f%%) currFitness = $currFitness%8.5f ...")
      }
      val cand = makeCandidateNetwork(curr)
      val candFitness = computeNetworkFitness(cand)

      if candFitness > currFitness then {
        println(f"@ $iterations: base: $currFitness%8.5f -> cand: $candFitness%8.5f")
        HalfAdderCommon.cases.foreach { case ((a1, a2, a3), (c, s)) =>
          val nnOutput = executeNetwork(cand, (a1, a2, a3))
          println(f"$a1 + $a2 + $a3 = $c $s: ${nnOutput._1}%8.5f, ${nnOutput._2}%8.5f"
                      + f";  ∆c = ${nnOutput._1 - c}%8.5f"
                      + f", ∆s = ${nnOutput._2 - s}%8.5f")
        }
        curr = cand
        currFitness = candFitness
      }
    }
    val endMs = System.currentTimeMillis()
    val durationMs = endMs - startMs
    println(s"for $iterations iterations, durationMs = $durationMs ms"
                + s" (${durationMs * 1.0 / iterations} ms/iteration)")
  }

}
