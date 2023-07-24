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

  def computeLayerActivation(prevLayerActivations: LayerActivations,
                             thisLayer           : LayerParameters
                            ): LayerActivations = {
    assert(thisLayer.biases.vector.size == thisLayer.weights.matrix.size)
    val thisLayerActivations =
      for {
        thisNeuronIdx <- thisLayer.biases.vector.indices
        thisNeuronBias = thisLayer.biases.vector(thisNeuronIdx)
        thisNeuronInputWeights = thisLayer.weights.matrix(thisNeuronIdx)
        _ = assert(prevLayerActivations.vector.size == thisNeuronInputWeights.size)
        sum = {
          // Optimization:  Avoids creating collection of products:  Was
          //   10-15% faster overall as of 2023-07-13:
          var accum: Double = 0d
          for (prevNeuronIdx <- prevLayerActivations.vector.indices) {
            accum +=
                prevLayerActivations.vector(prevNeuronIdx).raw
                    * thisNeuronInputWeights(prevNeuronIdx).raw
          }
          accum
        }
        rawAct = ActivationFunctions.standardLogisticFunction(sum + thisNeuronBias.raw)
        act = Activation(rawAct)
      } yield {
        act
      }
    LayerActivations(thisLayerActivations)
  }
}
