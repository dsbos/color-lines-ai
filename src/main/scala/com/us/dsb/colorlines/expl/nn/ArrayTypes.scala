package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn.ScalarTypes.{Activation, Bias, Weight}

/**
 * Common vector/matrix types.
 */
object ArrayTypes {

  case class LayerBiases(vector: IndexedSeq[Bias])

  /**
   * @param matrix
   * ... indexed by layer neuron offset, then by previous-layer neuron offset
   */
  case class LayerWeights(matrix: IndexedSeq[IndexedSeq[Weight]])

  case class LayerActivations(vector: IndexedSeq[Activation])


  def computeLayerActivation(prevLayerActivations: LayerActivations,
                             thisLayerBiases     : LayerBiases,
                             thisLayerWeights    : LayerWeights
                            ): LayerActivations = {
    assert(thisLayerBiases.vector.size == thisLayerWeights.matrix.size)
    val thisLayerActivations =
      for {
        thisNeuronIdx <- thisLayerBiases.vector.indices
        thisNeuronBias = thisLayerBiases.vector(thisNeuronIdx)
        thisNeuronInputWeights = thisLayerWeights.matrix(thisNeuronIdx)
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
