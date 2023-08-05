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

  // ?????? TODO:  Maybe parameterize:
  private val zeroOutputActivationRepresentation = 0.25 // 0
  private val oneOutputActivationRepresentation = 1 - zeroOutputActivationRepresentation

  private def bitOutputActivationRepresentation(bit: Byte): Double/*??????Activation?*/ = {
    val actOutDiff = oneOutputActivationRepresentation - zeroOutputActivationRepresentation
    zeroOutputActivationRepresentation + actOutDiff * bit
  }

  /**
   * Fitness function for network for half adder, give abstract execution function.
   */
  def computeFunctionFitness(executeNetwork: ExecutionFunction): Double = {
    val caseError =
      cases.map { case ((a1, a2, a3), (c, s)) =>
        val nnOutput = executeNetwork(a1, a2, a3)
        val cRepr = bitOutputActivationRepresentation(c)
        val sRepr = bitOutputActivationRepresentation(s)
        val cError = nnOutput._1 - cRepr
        val sError = nnOutput._2 - sRepr
        // ?????? TODO:  Maybe parameterize:
        val caseError = cError * cError + sError * sError
        //val error = math.abs(cError) + math.abs(sError)
        //maybe biggest difference
        caseError
      }
          .sum  // ?? maybe biggest error
    val fitness = -caseError
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
    var improvementCount = 0
    var lastImprovementInterationNumber = 0
    val maxIterations = 10_000_000
    var iteration = 0
    while iteration < maxIterations do {
      iteration += 1
      if (1 == iteration % 1_000_000) {
        val pct = 100.0 * iteration / maxIterations
        println(f"@ $iteration (/$maxIterations, ${pct}%4.1f%%):  currFitness = $currFitness%8.5f  (#$improvementCount) ...")
      }
      val cand = makeCandidateNetwork(curr)
      val candFitness = computeNetworkFitness(cand)

      if candFitness > currFitness then {
        improvementCount += 1
        lastImprovementInterationNumber = iteration
        println(f"@ $iteration: base: $currFitness%8.5g -> cand: $candFitness%8.5g (#$improvementCount)")
        if true then {
          HalfAdderCommon.cases.foreach { case ((a1, a2, a3), (c, s)) =>
            val nnOutput = executeNetwork(cand, (a1, a2, a3))
            val cRepr = bitOutputActivationRepresentation(c)
            val sRepr = bitOutputActivationRepresentation(s)
            println(f"$a1 + $a2 + $a3 = $c $s ($cRepr%4.2f, $sRepr%4.2f)"
                        + f": ${nnOutput._1}%8.5f, ${nnOutput._2}%8.5f"
                        + f";  ∆c = ${nnOutput._1 - cRepr}%8.5f"
                        + f", ∆s = ${nnOutput._2 - sRepr}%8.5f")
          }
        }
        curr = cand
        currFitness = candFitness
      }
    }
    val endMs = System.currentTimeMillis()
    val durationMs = endMs - startMs
    println(f"@ $iteration: result fitness: $currFitness ($improvementCount steps, last @ $lastImprovementInterationNumber)")
    HalfAdderCommon.cases.foreach { case ((a1, a2, a3), (c, s)) =>
      val nnOutput = executeNetwork(curr, (a1, a2, a3))
      val cRepr = bitOutputActivationRepresentation(c)
      val sRepr = bitOutputActivationRepresentation(s)
      println(f"$a1 + $a2 + $a3 = $c $s ($cRepr%4.2f, $sRepr%4.2f)"
                  + f": ${nnOutput._1}%8.5f, ${nnOutput._2}%8.5f"
                  + f";  ∆c = ${nnOutput._1 - cRepr}%8.5f"
                  + f", ∆s = ${nnOutput._2 - sRepr}%8.5f")
    }
    println(s"for $iteration iterations, durationMs = $durationMs ms"
                + s" (${durationMs * 1.0 / iteration} ms/iteration)")
  }

}
