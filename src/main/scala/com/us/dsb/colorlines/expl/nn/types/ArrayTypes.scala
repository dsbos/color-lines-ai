package com.us.dsb.colorlines.expl.nn.types

import com.us.dsb.colorlines.expl.nn2.ActivationComputation.computeNeuronActivation
import com.us.dsb.colorlines.expl.nn2.types.LowlevelTypes.{
  Activation, Bias, LayerActivations, Weight}

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

  //?????? revisit passing sizes, parameter order
  case class LayerParameters(size     : Int,
                             biases   : LayerBiases,
                             weights  : LayerWeights,
                             inputSize: Int) {
    assert(biases.vector.size == size)
    assert(weights.matrix.size == size)
    assert(weights.matrix.forall(_.size == inputSize))
  }

  def computeLayerActivation(prevLayerActivations: LayerActivations,
                             thisLayer           : LayerParameters,
                             activationFunction  : Double => Activation  //???????? Double or Activation?
                            ): LayerActivations = {
    assert(thisLayer.biases.vector.size == thisLayer.weights.matrix.size)
    val thisLayerActivations =
      for (neuronIdx <- thisLayer.biases.vector.indices) yield {
        computeNeuronActivation(inputActivations   = prevLayerActivations,
                                bias               = thisLayer.biases.vector(neuronIdx),
                                weights            = thisLayer.weights.matrix(neuronIdx),
                                activationFunction = activationFunction)
      }
    LayerActivations(thisLayerActivations)
  }
}
