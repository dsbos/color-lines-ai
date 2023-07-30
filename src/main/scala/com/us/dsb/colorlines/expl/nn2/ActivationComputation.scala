package com.us.dsb.colorlines.expl.nn2

import com.us.dsb.colorlines.expl.nn2.types.LowlevelTypes.{
  Activation, Bias, LayerActivations, Weight, raw}

object ActivationComputation {

  type ActivationFunction = Double => Activation  //???????? from Double or Activation?

  /**
   * Computes activation of a neuron.
   * @param inputActivations
   *   activations of input neurons; size and order must correspond to those of
   *   `weights`  //??????: ; _should_ be an `IndexedSeq` for speed
   * @param weights
   *   per-input weights of neuron; per input activation; _should_ be an
   *  `IndexedSeq` for speed
   * @param bias
   *   bias of neuron
   * @return
   *   ...
   */
  def computeNeuronActivation(inputActivations  : LayerActivations,  //??????? wrapped or just Seq[Activation]?
                              weights           : Seq[Weight],       //??????? wrapped or just Seq[Weight]?
                              bias              : Bias,
                              activationFunction: ActivationFunction
                             ): Activation = {
    require(inputActivations.vector.size == weights.size,
            s"inputActivations.vector.size = ${inputActivations.vector.size}"
                +  s" != weights.size = ${weights.size}")
    // Optimization:  Accumulating sum and using explicit while loop to avoid
    // creating collection of products or intermediate Tuples.  Over twice as
    // fast as .zip/.lazyZip, .map, and .sum.  (10-15% faster overall for
    // half-adder app as of 2023-07-13.)
    var sumAccum: Double = 0  //?????? add bias here?
    val end = inputActivations.vector.indices.end // avoiding re-accessing is significant
    var inputIdx = inputActivations.vector.indices.start
    while inputIdx < end do
      sumAccum += inputActivations.vector(inputIdx).raw * weights(inputIdx).raw
      inputIdx += 1
    activationFunction(sumAccum + bias.raw)
  }

}
