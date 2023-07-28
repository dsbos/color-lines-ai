package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn.ScalarTypes.{Activation, Bias, Weight}

/**
 * Common vector/matrix types.
 */
object ArrayTypes {

  case class LayerBiases(vector: IndexedSeq[Bias])
  object LayerBiases {
    def fill(layerSize: Int)(bias: => Bias): LayerBiases =
      LayerBiases(IndexedSeq.fill(layerSize)(bias))
  }

  /**
   * @param matrix
   * ... indexed by layer neuron offset, then by previous-layer neuron offset
   */
  case class LayerWeights(matrix: IndexedSeq[IndexedSeq[Weight]])
  object LayerWeights {
    def fill(layerSize: Int, prevLayerSize: Int)(weight: => Weight): LayerWeights =
      LayerWeights(IndexedSeq.fill(layerSize)(IndexedSeq.fill(prevLayerSize)(weight)))
  }

  case class LayerActivations(vector: IndexedSeq[Activation])

  //???????? revisit passing sizes, parameter order
  case class LayerParameters(size     : Int,
                             biases   : LayerBiases,
                             weights  : LayerWeights,
                             inputSize: Int) {
    assert(biases.vector.size == size)
    assert(weights.matrix.size == size)
    assert(weights.matrix.forall(_.size == inputSize))
  }

  /**
   * Computes activation of a neuron; uses standard logistic function.
   * @param inputActivations
   *   activations of input neurons; size and order must correspond to those of
   *   `weights`; _should_ be an `IndexedSeq` for speed
   * @param weights
   *   per-input weights of neuron; per input activation; _should_ be an
   *  `IndexedSeq` for speed
   * @param bias
   *   bias of neuron
   * @return
   */
  private def computeNeuronActivation(inputActivations: Seq[Activation],
                                      weights: Seq[Weight],
                                      bias: Bias
                                     ): Activation = {
    // Optimization:  Accumulating sum and using explicit while loop to avoid
    // creating collection of products or intermediate Tuples.  Over twice as
    // fast as .zip/.lazyZip, .map, and .sum.  (10-15% faster overall for
    // half-adder app as of 2023-07-13.)
    var sumAccum: Double = 0d
    {
      val end = inputActivations.indices.end  // avoiding re-accessing is significant
      var inputIdx = inputActivations.indices.start
      while inputIdx < end do
        sumAccum += inputActivations(inputIdx).raw * weights(inputIdx).raw
        inputIdx += 1
    }
    Activation(ActivationFunctions.standardLogisticFunction(sumAccum + bias.raw))
  }

  def computeLayerActivation(prevLayerActivations: LayerActivations,
                             thisLayer           : LayerParameters
                            ): LayerActivations = {
    assert(thisLayer.biases.vector.size == thisLayer.weights.matrix.size)
    val thisLayerActivations =
      for (neuronIdx <- thisLayer.biases.vector.indices) yield {
        computeNeuronActivation(inputActivations = prevLayerActivations.vector,
                                weights          = thisLayer.weights.matrix(neuronIdx),
                                bias             = thisLayer.biases.vector(neuronIdx))
      }
    LayerActivations(thisLayerActivations)
  }
}