package com.us.dsb.colorlines.expl.nn.types

import com.us.dsb.colorlines.expl.nn.ActivationFunctions
import com.us.dsb.colorlines.expl.nn2.types.ScalarTypes.{Activation, Bias, Weight}

/**
 * Common vector/matrix types.
 */
object ArrayTypes {

  /**
   * Biases of all neurons in a layer.
   * @param vector
   *    ; _should_ be an`IndexedSeq` for speed
   */
  case class LayerBiases(vector: Seq[Bias])
  object LayerBiases {
    def fill(layerSize: Int)(bias: => Bias): LayerBiases =
      LayerBiases(IndexedSeq.fill(layerSize)(bias))
  }

  /**
   * For all neurons in layer, all (input) weights of each neuron.
   *
   * @param matrix
   *   ... indexed by layer neuron offset, then by previous-layer neuron offset
   * @param matrix
   * ...; _should_ be an `IndexedSeq` for speed
   */
  case class LayerWeights(matrix: Seq[Seq[Weight]])
  object LayerWeights {
    def fill(layerSize: Int, prevLayerSize: Int)(weight: => Weight): LayerWeights =
      LayerWeights(IndexedSeq.fill(layerSize)(IndexedSeq.fill(prevLayerSize)(weight)))
  }

  /**
   *
   * @param vector
   *   ; _should_ be an `IndexedSeq` for speed
   */
  case class LayerActivations(vector: Seq[Activation])

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
  // ?????? TODO:  Pass in activation function.
  private def computeNeuronActivation(inputActivations: Seq[Activation],
                                      weights: Seq[Weight],
                                      bias: Bias,
                                      activationFunction: Double => Activation  //???????? Double or Activation?
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
    activationFunction(sumAccum + bias.raw)
  }

  def computeLayerActivation(prevLayerActivations: LayerActivations,
                             thisLayer           : LayerParameters,
                             activationFunction  : Double => Activation  //???????? Double or Activation?
                            ): LayerActivations = {
    assert(thisLayer.biases.vector.size == thisLayer.weights.matrix.size)
    val thisLayerActivations =
      for (neuronIdx <- thisLayer.biases.vector.indices) yield {
        computeNeuronActivation(inputActivations   = prevLayerActivations.vector,
                                bias               = thisLayer.biases.vector(neuronIdx),
                                weights            = thisLayer.weights.matrix(neuronIdx),
                                activationFunction = activationFunction)
      }
    LayerActivations(thisLayerActivations)
  }
}
